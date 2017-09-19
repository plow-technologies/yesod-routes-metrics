{-# LANGUAGE OverloadedStrings #-}

module Yesod.Routes.Metrics (
   YesodMetrics(..)
 , YesodMetricsConfig(..)
 , defaultYesodMetricsConfig
 , spacedYesodMetricsConfig
 , addSpacesToRoute
 , removeUnderlines
 , registerYesodMetricsMkMetricsFunction
 , registerYesodMetrics
 , registerYesodMetricsWithResourceTrees
 , metrics
 ) where

import Yesod.Routes.Convert.Internal
import Yesod.Routes.Parser.Internal

import           Control.Monad   (forM)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Char (isUpper)
import           Data.Int (Int64)
import           Data.Maybe (maybe)
import           Data.Monoid     ((<>))
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time
import           Network.HTTP.Types.Status   (statusCode)
import           Network.Wai
import           System.Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge   as G
import           Yesod.Routes.TH.Types (ResourceTree)

import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)

data YesodMetrics = 
  YesodMetrics
    { routeCounters         :: Map.Map String Counter.Counter
    , routesTotalRequests   :: Counter.Counter
    , routeMaxLatencies     :: Map.Map String G.Gauge
    , routeMinLatencies     :: Map.Map String G.Gauge
    , routeAverageLatencies :: Map.Map String G.Gauge
    , routeLatencySum       :: Map.Map String (IORef Int64)  -- used for calculation only with its routeCounter to calculate average
    }

data YesodMetricsConfig =
  YesodMetricsConfig
    { namespace      :: Text
    , verbose        :: Bool
    , underlined     :: Bool
    , alterRouteName :: (String -> String)
    }

defaultYesodMetricsConfig :: YesodMetricsConfig
defaultYesodMetricsConfig = YesodMetricsConfig "" True True id

spacedYesodMetricsConfig :: YesodMetricsConfig
spacedYesodMetricsConfig = YesodMetricsConfig "" True False addSpacesToRoute

-- | Make route names more readable by adding spaces.
addSpacesToRoute :: String -> String
addSpacesToRoute = concat . fmap (\x -> if isUpper x then (" " ++ [x]) else [x])

removeUnderlines :: String -> String
removeUnderlines = 
  fmap (\c -> 
    case c of 
      '_' -> ' '
      _   -> c
    )


registerYesodMetricsMkMetricsFunction :: YesodMetricsConfig -> ByteString -> Store -> IO Middleware
registerYesodMetricsMkMetricsFunction config routesFileContents store = do
  ym <- registerYesodMetrics config routesFileContents store
  return $ metrics config routesFileContents ym

registerYesodMetrics :: YesodMetricsConfig -> ByteString -> Store -> IO YesodMetrics
registerYesodMetrics config routesFileContents store = do
  registerYesodMetricsWithResourceTrees config resources store  
  where
    resources = resourcesFromString . C.unpack $ routesFileContents

registerYesodMetricsWithResourceTrees :: YesodMetricsConfig -> [ResourceTree String] -> Store -> IO YesodMetrics
registerYesodMetricsWithResourceTrees ymc resources store = do
  counters <- forM routes $ \route -> do
    let namespacedRoute = namespace' <> (T.pack route)
    counter <- createCounter namespacedRoute store
    if verbose ymc
      then do
        counters <- mapM (\rs -> createCounter (namespacedRoute <> rs) store) responseStatuses'
        return $ [(route, counter)] ++ zip ((T.unpack . (<>) (T.pack route)) <$> responseStatuses') counters
      else 
        return [(route, counter)]
  
  c <- createCounter (T.pack $ alterResponseStatus "total_requests_per_interval") store
  avgs <- forM routes $ \route -> do
    r <- newIORef 0
    return (route,r)
    
  return $ YesodMetrics (Map.fromList $ concat counters) c Map.empty Map.empty Map.empty (Map.fromList avgs)

  where
    routes = (alterRouteName ymc) <$> convertResourceTreesToRouteNames resources
    
    responseStatuses  = (\n -> "_response_status_" ++ (show n) ++ "xx") <$> ([1..5] :: [Int])
    
    alterResponseStatus =
      if underlined ymc
        then id
        else removeUnderlines
                      
    responseStatuses' = T.pack . alterResponseStatus <$> responseStatuses
    
    -- append a '.' to a given namespace, if not empty
    namespace'
      | T.null (namespace ymc) = ""
      | otherwise = (namespace ymc) <> "."

metrics :: YesodMetricsConfig -> ByteString -> YesodMetrics -> Middleware
metrics ymc routesFileContents yesodMetrics app req respond =
  metricsWithResourceTrees ymc resources yesodMetrics app req respond
  where 
    resources  = resourcesFromString . C.unpack $ routesFileContents

metricsWithResourceTrees :: YesodMetricsConfig -> [ResourceTree String] -> YesodMetrics -> Middleware
metricsWithResourceTrees ymc resources yesodMetrics app req respond = do  
  before <- getCurrentTime
  
  app req $ respond' before

  where 
    mRouteName = convertRequestToRouteName req resources

    respond' :: UTCTime -> Response -> IO ResponseReceived
    respond' before res = do 
      -- if the path in req corresponds to one of the Yesod routes
      -- then update its corresponding counter      
      case mRouteName of 
        Nothing -> return ()
        Just routeName -> do
          after <- getCurrentTime
          
          updateRoute ((alterRouteName ymc) routeName)
          updateTotalRequests
          
          let diff = (round $ 1000 * (diffUTCTime after before))
          updateMaxLatency ((alterRouteName ymc) routeName) diff
          updateMinLatency ((alterRouteName ymc) routeName) diff
          updateAverageLatency ((alterRouteName ymc) routeName) diff
          
          case statusCode $ responseStatus res of
            s | s >= 500  -> updateRoute (routeName' <> (alterResponseStatus "_response_status_5xx"))
              | s >= 400  -> updateRoute (routeName' <> (alterResponseStatus "_response_status_4xx"))
              | s >= 300  -> updateRoute (routeName' <> (alterResponseStatus "_response_status_3xx"))
              | s >= 200  -> updateRoute (routeName' <> (alterResponseStatus "_response_status_2xx"))
              | otherwise -> updateRoute (routeName' <> (alterResponseStatus "_response_status_1xx"))
          where
            routeName' = (alterRouteName ymc) routeName
            alterResponseStatus =
              if underlined ymc
                then id
                else removeUnderlines
      respond res
    
    updateRoute name = 
      case Map.lookup name (routeCounters yesodMetrics) of
        Nothing -> return ()
        Just counter ->
          Counter.inc counter
    
    updateTotalRequests = Counter.inc (routesTotalRequests yesodMetrics)
    
    updateMaxLatency :: String -> Int64 -> IO ()
    updateMaxLatency name newLatency = do
      maybe 
        (return ())
        (\gauge -> G.read gauge >>= \oldLatency -> if newLatency > oldLatency then G.set gauge newLatency else return ())
        (Map.lookup name (routeMaxLatencies yesodMetrics))

    updateMinLatency :: String -> Int64 -> IO ()
    updateMinLatency name newLatency = 
      maybe 
        (return ())
        (\gauge -> G.read gauge >>= \oldLatency -> if newLatency < oldLatency then G.set gauge newLatency else return ())
        (Map.lookup name (routeMinLatencies yesodMetrics))
    
    updateAverageLatency :: String -> Int64 -> IO ()
    updateAverageLatency name newLatency = do
      case (,,) <$> mc <*> ms <*> ma of
        Nothing -> return ()
        Just (c,latencySumRef,avgGauge) -> do
          t <- Counter.read c
          atomicModifyIORef latencySumRef (\ls -> (ls + newLatency, ()))
          latencySum <- readIORef latencySumRef
          G.set avgGauge (round $ (fromIntegral latencySum :: Double) / (fromIntegral t :: Double))
      where 
        mc = (Map.lookup name (routeCounters   yesodMetrics))
        ms = (Map.lookup name (routeLatencySum yesodMetrics))
        ma = (Map.lookup name (routeAverageLatencies yesodMetrics))

{-# LANGUAGE OverloadedStrings #-}

module Yesod.Routes.Metrics (
   YesodMetrics(..)
 , YesodMetricsConfig(..)
 , resetYesodMetricsGauges
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

import           Control.Monad   (forM, void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Char (isUpper)
import           Data.Int (Int64)
import           Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import           Data.Maybe (maybe)
import           Data.Monoid     ((<>))
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import           Network.HTTP.Types.Status   (statusCode)
import           Network.Wai
import           System.Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge   as G
import           Yesod.Routes.Stats
import           Yesod.Routes.TH.Types (ResourceTree)


data YesodMetrics = 
  YesodMetrics
    { routesTotalRequests   :: Counter.Counter               -- number of requests for the entire system
    , routeCounters         :: Map.Map String Counter.Counter
    , routeMaxLatencies     :: Map.Map String G.Gauge
    , routeMinLatencies     :: Map.Map String G.Gauge
    , routeAverageLatencies :: Map.Map String G.Gauge
    , route50thPercentiles  :: Map.Map String G.Gauge
    , route75thPercentiles  :: Map.Map String G.Gauge
    , route90thPercentiles  :: Map.Map String G.Gauge
    , route99thPercentiles  :: Map.Map String G.Gauge
    , routeLatencies        :: Map.Map String (IORef (VU.Vector Double))
    }


data YesodMetricsHelper = 
  YesodMetricsHelper
    { routeCounters'         :: [(String, Counter.Counter)]
    , routeMaxLatencies'     :: (String, G.Gauge)
    , routeMinLatencies'     :: (String, G.Gauge)
    , routeAverageLatencies' :: (String, G.Gauge)
    , route50thPercentiles'  :: (String, G.Gauge)
    , route75thPercentiles'  :: (String, G.Gauge)
    , route90thPercentiles'  :: (String, G.Gauge)
    , route99thPercentiles'  :: (String, G.Gauge)
    , routeLatencies'        :: (String, (IORef (VU.Vector Double)))
    }


data YesodMetricsConfig =
  YesodMetricsConfig
    { namespace      :: Text
    , verbose        :: Bool
    , underlined     :: Bool
    , alterRouteName :: (String -> String)
    }

resetYesodMetricsGauges :: YesodMetrics -> IO ()
resetYesodMetricsGauges ym = do
  mapM_ (\g -> void $ G.set g 0) $ routeMaxLatencies ym
  mapM_ (\g -> void $ G.set g 0) $ routeMinLatencies ym
  mapM_ (\g -> void $ G.set g 0) $ routeAverageLatencies ym
  mapM_ (\g -> void $ G.set g 0) $ route50thPercentiles ym
  mapM_ (\g -> void $ G.set g 0) $ route75thPercentiles ym
  mapM_ (\g -> void $ G.set g 0) $ route90thPercentiles ym
  mapM_ (\g -> void $ G.set g 0) $ route99thPercentiles ym

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
  -- (counters,maxs,mins,avgs,latencies)
  dats <- forM routes $ \route -> do
    let namespacedRoute = namespace' <> (T.pack route)
    
    counter    <- createCounter namespacedRoute store
    counters <- 
      if verbose ymc
        then do
          counters <- mapM (\rs -> createCounter (namespacedRoute <> rs) store) responseStatuses'
          return $ [(route, counter)] ++ zip ((T.unpack . (<>) (T.pack route)) <$> responseStatuses') counters
        else 
          return [(route, counter)]

    maxLatency    <- createGauge (namespacedRoute <> (T.pack . alterResponseStatus $ "_max_latency")) store
    minLatency    <- createGauge (namespacedRoute <> (T.pack . alterResponseStatus $ "_min_latency")) store
    avgLatency    <- createGauge (namespacedRoute <> (T.pack . alterResponseStatus $ "_avg_latency")) store
    p50Latency    <- createGauge (namespacedRoute <> (T.pack . alterResponseStatus $ "_50th_percentile_latency")) store
    p75Latency    <- createGauge (namespacedRoute <> (T.pack . alterResponseStatus $ "_75th_percentile_latency")) store
    p90Latency    <- createGauge (namespacedRoute <> (T.pack . alterResponseStatus $ "_90th_percentile_latency")) store
    p99Latency    <- createGauge (namespacedRoute <> (T.pack . alterResponseStatus $ "_99th_percentile_latency")) store
    latencies     <- newIORef VU.empty

    return $
     YesodMetricsHelper
       counters
       (route,maxLatency)
       (route,minLatency)
       (route,avgLatency)
       (route,p50Latency)
       (route,p75Latency)
       (route,p90Latency)
       (route,p99Latency)       
       (route,latencies)
  
  totalRequestsCounter <- createCounter (T.pack $ alterResponseStatus "total_requests_per_interval") store
    
  return $ 
    YesodMetrics
      totalRequestsCounter
      (Map.fromList $ concat $ routeCounters' <$> dats) 
      (Map.fromList $ routeMaxLatencies'      <$> dats) 
      (Map.fromList $ routeMinLatencies'      <$> dats) 
      (Map.fromList $ routeAverageLatencies'  <$> dats)
      (Map.fromList $ route50thPercentiles'   <$> dats)
      (Map.fromList $ route75thPercentiles'   <$> dats)
      (Map.fromList $ route90thPercentiles'   <$> dats)
      (Map.fromList $ route99thPercentiles'   <$> dats)
      (Map.fromList $ routeLatencies'         <$> dats)
      
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
          
          let latency = (round $ 1000 * (diffUTCTime after before))
          updateMaxLatency ((alterRouteName ymc) routeName) latency
          updateMinLatency ((alterRouteName ymc) routeName) latency
          updateAvgAndPercentiles ((alterRouteName ymc) routeName) latency
          
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
    
    -- The min_latency gauge will be initiated to 0. We should ignore this 
    -- value the first time we receive something.
    updateMinLatency :: String -> Int64 -> IO ()
    updateMinLatency name newLatency = 
      maybe 
        (return ())
        (\gauge -> G.read gauge >>= \oldLatency -> if (newLatency < oldLatency || oldLatency <= 0) then G.set gauge newLatency else return ())
        (Map.lookup name (routeMinLatencies yesodMetrics))
    
    updateAvgAndPercentiles :: String -> Int64 -> IO ()
    updateAvgAndPercentiles name newLatency = do
      case Map.lookup name (routeLatencies yesodMetrics) of
        Nothing -> return ()
        Just latenciesVectorIORef -> do
          atomicModifyIORef latenciesVectorIORef (\ls -> (VU.snoc ls (fromIntegral newLatency), ()))
          latenciesVector <- readIORef latenciesVectorIORef
          latenciesVectorMutable <- VG.basicUnsafeThaw latenciesVector
          maybe (return ()) (\gauge -> G.set gauge $ round $ mean latenciesVector) (Map.lookup name (routeAverageLatencies yesodMetrics))
          p50 <- percentileWithSort 0.5 latenciesVectorMutable
          p75 <- percentileWithSort 0.75 latenciesVectorMutable
          p90 <- percentileWithSort 0.90 latenciesVectorMutable
          p99 <- percentileWithSort 0.99 latenciesVectorMutable
          maybe (return ()) (\gauge -> G.set gauge (round p50)) (Map.lookup name (route50thPercentiles yesodMetrics))
          maybe (return ()) (\gauge -> G.set gauge (round p75)) (Map.lookup name (route75thPercentiles yesodMetrics))
          maybe (return ()) (\gauge -> G.set gauge (round p90)) (Map.lookup name (route90thPercentiles yesodMetrics))
          maybe (return ()) (\gauge -> G.set gauge (round p99)) (Map.lookup name (route99thPercentiles yesodMetrics))

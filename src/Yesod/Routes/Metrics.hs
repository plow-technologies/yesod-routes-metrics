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
 
 , percentile
 , quantileIO
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


import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM



{-
import qualified Data.Vector as IV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Algorithms.Intro as VA
-}

-- | provide 0.0 - 1.0
percentile :: Double -> VU.Vector Double -> Double
percentile = quantile

quantileIO :: Double -> VUM.IOVector Double -> IO Double
quantileIO quant s = do 
  VA.sort s
  iv <- VG.unsafeFreeze s :: IO (VU.Vector Double)
  return $ quantile quant iv

quantile :: Double -> VU.Vector Double -> Double
quantile quant s
    | VU.length s == 0 = 0
    | pos > fromIntegral (VU.length s) = VU.last s
    | pos' < 1 = VU.head s
    | otherwise =
        lower + (pos - fromIntegral (floor pos :: Int)) * (upper - lower)
  where
    q = clamp quant
    pos = q * (1 + fromIntegral (VU.length s))
    pos' = truncate pos
    lower = VU.unsafeIndex s (pos' - 1)
    upper = VU.unsafeIndex s pos'

clamp :: Double -> Double
clamp x | x > 1 = 1
        | x < 0 = 0
        | otherwise = x
{-# INLINE clamp #-}


data YesodMetrics = 
  YesodMetrics
    { routesTotalRequests   :: Counter.Counter               -- number of requests for the entire system
    , routeCounters         :: Map.Map String Counter.Counter
    , routeMaxLatencies     :: Map.Map String G.Gauge
    , routeMinLatencies     :: Map.Map String G.Gauge
    , routeAverageLatencies :: Map.Map String G.Gauge
    , routeLatencies        :: Map.Map String (IORef (Vector Double))
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


fst4 :: (a,b,c,d,e,f) -> a
fst4 (a,_,_,_,_,_) = a

snd4 :: (a,b,c,d,e,f) -> b
snd4 (_,b,_,_,_,_) = b

thrd4 :: (a,b,c,d,e,f) -> c
thrd4 (_,_,c,_,_,_) = c

frth4 :: (a,b,c,d,e,f) -> d
frth4 (_,_,_,d,_,_) = d

ffth5 :: (a,b,c,d,e,f) -> e
ffth5 (_,_,_,_,e,_) = e

sxth6 :: (a,b,c,d,e,f) -> f
sxth6 (_,_,_,_,_,f) = f

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
    latencies     <- newIORef V.empty
    latencySumRef <- newIORef 0

    return ( counters
           , (route,maxLatency)
           , (route,minLatency)
           , (route,avgLatency)
           , (route,latencies)
           , (route,latencySumRef)
           )
  
  totalRequestsCounter <- createCounter (T.pack $ alterResponseStatus "total_requests_per_interval") store
    
  return $ 
    YesodMetrics
      totalRequestsCounter
      (Map.fromList $ concat $ fst4 <$> dats) 
      (Map.fromList $ snd4  <$> dats) 
      (Map.fromList $ thrd4 <$> dats) 
      (Map.fromList $ frth4 <$> dats)
      (Map.fromList $ ffth5 <$> dats)
      (Map.fromList $ sxth6 <$> dats)

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


-- collect percentiles 90 95 99
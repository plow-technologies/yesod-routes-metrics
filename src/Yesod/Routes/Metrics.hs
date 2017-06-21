{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Yesod.Routes.Metrics (
   convertResourceTreesToRouteNames
 , convertRequestToRouteName
 , resourcesFromString
 
 , metrics
 , registerYesodMetrics
 ) where

import Yesod.Routes.Convert.Internal
import Yesod.Routes.Parser.Internal

--import           Control.Applicative
import           Control.Monad (forM)
import           Data.Monoid                 ((<>))
import qualified Data.Map.Strict as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock
--import           Network.HTTP.Types.Status   (statusCode)
import           Network.Wai
import           System.Metrics
import qualified System.Metrics.Counter      as Counter
--import qualified System.Metrics.Distribution as Distribution
import           Yesod.Routes.TH.Types (ResourceTree)


data YesodMetrics = 
  YesodMetrics 
    { routeCounters :: Map.Map String Counter.Counter
    }

registerYesodMetrics :: Text -> [String] -> Store -> IO YesodMetrics
registerYesodMetrics namespace routes store = do
  counters <- forM routes $ \route -> do 
    counter <- createCounter (namespace' <> (T.pack route)) store
    return (route, counter)
  return $ YesodMetrics (Map.fromList counters)
  {-
  WaiMetrics
    <$> createCounter      (namespace' <> "wai.request_count")        store
    <*> createDistribution (namespace' <> "wai.latency_distribution") store
    <*> createCounter      (namespace' <> "wai.response_status_1xx")  store
    <*> createCounter      (namespace' <> "wai.response_status_2xx")  store
    <*> createCounter      (namespace' <> "wai.response_status_3xx")  store
    <*> createCounter      (namespace' <> "wai.response_status_4xx")  store
    <*> createCounter      (namespace' <> "wai.response_status_5xx")  store
  -}
  where
    -- append a '.' to a given namespace, if not empty
    namespace'
      |T.null namespace = namespace
      |otherwise = namespace <> "."

-- type Middleware = Application -> Application
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
metrics :: [ResourceTree String] -> YesodMetrics -> Middleware
metrics rts yesodMetrics app req respond = do
  -- Counter.inc (requestCounter waiMetrics)
  let mRouteName = convertRequestToRouteName req rts
  case mRouteName of
    Nothing        -> return ()
    Just routeName -> 
      case Map.lookup routeName (routeCounters yesodMetrics) of
        Nothing -> return ()
        Just counter -> Counter.inc counter
  start <- getCurrentTime
  
  app req (respond' start)
    where respond' :: UTCTime -> Response -> IO ResponseReceived
          respond' _start res = do
            {-
            Counter.inc $ case statusCode $ responseStatus res of
              s | s >= 500  -> statusCode500Counter waiMetrics
                | s >= 400  -> statusCode400Counter waiMetrics
                | s >= 300  -> statusCode300Counter waiMetrics
                | s >= 200  -> statusCode200Counter waiMetrics
                | otherwise -> statusCode100Counter waiMetrics
            -}
            -- end <- getCurrentTime
            -- Distribution.add (latencyDistribution waiMetrics) (realToFrac $ diffUTCTime end start)
            respond res
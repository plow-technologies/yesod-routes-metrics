{-# LANGUAGE OverloadedStrings #-}

module Yesod.Routes.Metrics (
   YesodMetrics(..)
 , registerYesodMetrics
 , registerYesodMetricsWithResourceTrees
 , metrics
 ) where

import Yesod.Routes.Convert.Internal
import Yesod.Routes.Parser.Internal

import           Control.Monad   (forM)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Monoid     ((<>))
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Network.HTTP.Types.Status   (statusCode)
import           Network.Wai
import           System.Metrics
import qualified System.Metrics.Counter as Counter
import           Yesod.Routes.TH.Types (ResourceTree)

data YesodMetrics = 
  YesodMetrics 
    { routeCounters :: Map.Map String Counter.Counter
    }

registerYesodMetrics :: Bool -> Text -> ByteString -> Store -> IO YesodMetrics
registerYesodMetrics verbose namespace routesFileContents store = do
  registerYesodMetricsWithResourceTrees verbose namespace resources store  
  where
    resources = resourcesFromString . C.unpack $ routesFileContents

registerYesodMetricsWithResourceTrees :: Bool -> Text -> [ResourceTree String] -> Store -> IO YesodMetrics
registerYesodMetricsWithResourceTrees verbose namespace resources store = do
  counters <- forM routes $ \route -> do 
    let namespacedRoute = namespace' <> (T.pack route)
    counter <- createCounter namespacedRoute store
    if verbose
      then do
        counter1xx <- createCounter (namespacedRoute <> "_response_status_1xx") store
        counter2xx <- createCounter (namespacedRoute <> "_response_status_2xx") store
        counter3xx <- createCounter (namespacedRoute <> "_response_status_3xx") store
        counter4xx <- createCounter (namespacedRoute <> "_response_status_4xx") store
        counter5xx <- createCounter (namespacedRoute <> "_response_status_5xx") store
 
        return  [ (route, counter)
                , ((route <> "_response_status_1xx"), counter1xx)
                , ((route <> "_response_status_2xx"), counter2xx)
                , ((route <> "_response_status_3xx"), counter3xx)
                , ((route <> "_response_status_4xx"), counter4xx)
                , ((route <> "_response_status_5xx"), counter5xx)
                ]
      else 
        return [(route, counter)]
        
  return $ YesodMetrics (Map.fromList $ concat counters)

  where
    routes = convertResourceTreesToRouteNames resources
    -- append a '.' to a given namespace, if not empty
    namespace'
      | T.null namespace = namespace
      | otherwise = namespace <> "."

metrics :: ByteString -> YesodMetrics -> Middleware
metrics routesFileContents yesodMetrics app req respond =
  metricsWithResourceTrees resources yesodMetrics app req respond
  where 
    resources  = resourcesFromString . C.unpack $ routesFileContents

metricsWithResourceTrees :: [ResourceTree String] -> YesodMetrics -> Middleware
metricsWithResourceTrees resources yesodMetrics app req respond = do
  -- if the path in req corresponds to one of the Yesod routes
  -- then update its corresponding counter
  case mRouteName of
    Nothing        -> return ()
    Just routeName -> updateRoute routeName
          
  app req $ respond'

  where 
    mRouteName = convertRequestToRouteName req resources

    respond' :: Response -> IO ResponseReceived
    respond' res = do 
      case mRouteName of 
        Nothing -> return ()
        Just routeName ->
          case statusCode $ responseStatus res of
            s | s >= 500  -> updateRoute (routeName <> "_response_status_5xx")
              | s >= 400  -> updateRoute (routeName <> "_response_status_4xx")
              | s >= 300  -> updateRoute (routeName <> "_response_status_3xx")
              | s >= 200  -> updateRoute (routeName <> "_response_status_2xx")
              | otherwise -> updateRoute (routeName <> "_response_status_1xx")
      respond res
    
    updateRoute name = 
      case Map.lookup name (routeCounters yesodMetrics) of
        Nothing -> return ()
        Just counter -> Counter.inc counter

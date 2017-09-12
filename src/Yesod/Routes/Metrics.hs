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
        
  return $ YesodMetrics (Map.fromList $ concat counters)

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
  -- if the path in req corresponds to one of the Yesod routes
  -- then update its corresponding counter
  case mRouteName of
    Nothing        -> return ()
    Just routeName -> updateRoute $ (alterRouteName ymc) routeName
          
  app req $ respond'

  where 
    mRouteName = convertRequestToRouteName req resources

    respond' :: Response -> IO ResponseReceived
    respond' res = do 
      case mRouteName of 
        Nothing -> return ()
        Just routeName ->
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
        Just counter -> Counter.inc counter

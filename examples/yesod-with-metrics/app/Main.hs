{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.Concurrent (forkIO, threadDelay)
import           Data.ByteString.Char8 (unpack)
import           Data.ByteString (ByteString)
import           Data.FileEmbed
import           Data.Text (Text)
import qualified Data.Text as T
import           Foundation
import           Network.Wai.Handler.Warp (run)
import           System.Metrics           (newStore, sampleAll)
import           Yesod
import           Yesod.Routes.Metrics

mkYesodDispatch "App" resourcesApp

routesFile :: ByteString
routesFile = $(embedFile "config/routes")

getTestR :: Handler Text
getTestR = return "Hello World!"

getPersonR :: Text -> Handler Html
getPersonR name = defaultLayout [whamlet|<h1>Hello #{name}!|]

handleDateR :: Integer -> Text -> Int -> Handler Text -- text/plain
handleDateR year month day =
    return $
        T.concat [month, " ", T.pack $ show day, ", ", T.pack $ show year]

getWikiR :: [Text] -> Handler Text
getWikiR = return . T.unwords

main :: IO ()
main = do 
  let resources  = resourcesFromString $ unpack routesFile
      routeNames = convertResourceTreesToRouteNames resources
  
  app <- toWaiApp App
  store <- newStore
  yesodMetrics <- registerYesodMetrics "test" routeNames store
  
  _ <- forkIO $ loop store
  
  run 3000 (metrics resources yesodMetrics $ app)
  
  where 
    loop store = do
      sample <- sampleAll store
      print sample      
      threadDelay 1000000
      loop store
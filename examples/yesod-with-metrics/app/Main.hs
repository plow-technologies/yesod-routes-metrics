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
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Foundation
import           Network.Wai.Handler.Warp (run)
import           System.Metrics           (newStore, registerGcMetrics, sampleAll)
import           System.Random            (randomRIO)
import           Yesod
import qualified Yesod.Routes.Metrics as Yesod
import           System.Remote.Monitoring (forkServerWith)

mkYesodDispatch "App" resourcesApp

routesFileContents :: ByteString
routesFileContents = $(embedFile "config/routes")

getHomeR :: Handler Text
getHomeR = return "Hello World!"

getDelayR :: Int -> Int -> Handler Text
getDelayR start stop = do
  waitTime <- liftIO $ randomRIO (start,stop)
  liftIO $ threadDelay waitTime -- uses microseconds
  return $ T.pack . show $ waitTime

postNewUserR :: Handler Text
postNewUserR = return $ "Making a new user"

getUserR :: Int -> Handler Text
getUserR userId = return $ "User with id: " <> (T.pack . show $ userId)

deleteUserR :: Int -> Handler Text
deleteUserR userId = return $ "Deleted user with id: " <> (T.pack . show $ userId)

main :: IO ()
main = do
  app <- toWaiApp App
  store <- newStore
  (_,yesodMetricsF) <- Yesod.registerYesodMetricsMkMetricsFunction Yesod.spacedYesodMetricsConfig routesFileContents store
  registerGcMetrics store
  
  -- print store contents
  _ <- forkIO $ loop store
  
  _ <- forkServerWith store "localhost" 7000
  
  run 3000 (yesodMetricsF $ app)
  
  where
    -- see store updates on the server side
    loop store = do
      sample <- sampleAll store
      print sample      
      threadDelay 5000000
      loop store
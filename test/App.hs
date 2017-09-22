{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module App (runYesodServer) where

import           Control.Concurrent (forkIO, threadDelay)
import           Data.ByteString.Char8 (unpack)
import           Data.ByteString (ByteString)
import           Data.FileEmbed
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Foundation
import           Network.Wai.Handler.Warp (run)
import           System.Metrics           (newStore, registerGcMetrics, sampleAll, Store)
import           System.Random            (randomRIO)
import           System.Remote.Monitoring (forkServerWith)
import           Yesod
import qualified Yesod.Routes.Metrics as Yesod

mkYesodDispatch "App" resourcesApp

routesFileContents :: ByteString
routesFileContents = $(embedFile "test/config/routes")

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

runYesodServer :: Int -> Yesod.YesodMetricsConfig -> Store -> IO ()
runYesodServer port ymc store = do
  app <- toWaiApp App
  yesodMetricsF <- Yesod.registerYesodMetricsMkMetricsFunction ymc routesFileContents store
  registerGcMetrics store  
  run port (yesodMetricsF $ app)
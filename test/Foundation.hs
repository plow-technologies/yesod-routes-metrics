{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Foundation where

import           Data.Text (Text)
import           Yesod
  
data App = App
instance Yesod App

mkYesodData "App" $(parseRoutesFile "test/config/routes")
module Main where
  
import Yesod.Routes.Metrics (resourcesFromString)
import Yesod.Routes.Util (showResourceTree)
main :: IO ()
main = do 
  ps <- readFile "routes"
  mapM_ print $ showResourceTree <$> resourcesFromString ps
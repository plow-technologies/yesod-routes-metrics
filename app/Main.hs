module Main where
  
import Yesod.Routes.Metrics (resourcesFromString, showResourceTree)

main :: IO ()
main = do 
  ps <- readFile "routes"
  mapM_ print $ showResourceTree <$> resourcesFromString ps
  print "hi"
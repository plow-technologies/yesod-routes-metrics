module Main where

import Yesod.Routes.Convert
import Yesod.Routes.Util
--convertResourceTreesToRouteNames
--, convertRequestToRouteName
--, resourcesFromString
main :: IO ()
main = do 
  f <- readFile "config/routes"
  let resources = resourcesFromString f
  putStrLn ""
  putStrLn "config/routes ResourceTrees:"
  mapM_ (putStrLn . showResourceTree) resources
  putStrLn ""
  putStrLn "config/routes route names:"
  mapM_ putStrLn $ convertResourceTreesToRouteNames resources
  putStrLn ""
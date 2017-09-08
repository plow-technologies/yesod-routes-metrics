module Yesod.Routes.Util where

import           Data.Char (isUpper)  
import           Yesod.Routes.TH.Types

-- useful for testing and debugging
showResourceTree :: (Show a) => ResourceTree a -> String
showResourceTree (ResourceLeaf rl)            = 
  "ResourceLeaf: " ++ show rl
showResourceTree (ResourceParent s co ps rts) = 
  "ResourceParent: " ++ (show s) ++ ", CheckOverlap: " ++ (show co) ++ (concat $ showPiece <$> ps) ++ "\n" ++ (concat $ showResourceTree <$> rts)
  where 
    showPiece p = 
      ", " ++ show p

prettyPrintRouteName :: String -> String
prettyPrintRouteName =  concat . fmap (\c -> if isUpper c then (" " ++ [c]) else [c])
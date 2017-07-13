module Yesod.Routes.Convert.Internal where 

import           Data.Char (toLower)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import           Data.List (concat)
import           Network.Wai
import           Yesod.Routes.TH.Types


-- parse a route file, then get the route names
convertResourceTreesToRouteNames :: [ResourceTree a] -> [String]
convertResourceTreesToRouteNames = concat . fmap convertResourceTreeToRouteNames

convertResourceTreeToRouteNames :: ResourceTree a -> [String]
convertResourceTreeToRouteNames (ResourceLeaf rl@(Resource _ _ (Methods _ _) _ _)) = (\m -> (toLower <$> m) ++ (resourceName rl)) <$> (methodsMethods . resourceDispatch $ rl)
convertResourceTreeToRouteNames _ = []


-- Wai Request and yesod-core ResourceTree to Maybe String
convertRequestToRouteName :: Request -> [ResourceTree a] -> Maybe String
convertRequestToRouteName req resourceTrees =
  case mResourceTree of
    Nothing -> Nothing
    Just rt -> convertMethodToRouteName (BS.unpack $ requestMethod req) rt
  where
    mResourceTree = convertRequestToResourceTree req resourceTrees

convertMethodToRouteName :: String -> ResourceTree a -> Maybe String
convertMethodToRouteName method rt@(ResourceLeaf rl) =
  if methodInResource method rt
    then Just $ (toLower <$> method) ++ (resourceName rl)
    else Nothing
convertMethodToRouteName _ _ = Nothing

convertRequestToResourceTree :: Request -> [ResourceTree a] -> Maybe (ResourceTree a)
convertRequestToResourceTree req resourceTrees = convertMethodAndPathToResourceTree reqMethod reqPaths resourceTrees
  where 
    reqMethod = BS.unpack $ requestMethod req
    reqPaths  = T.unpack <$> pathInfo req

-- list of resource tree 
-- find first that matches, otherwise return nothing
convertMethodAndPathToResourceTree :: String -> [String] -> [ResourceTree a] -> Maybe (ResourceTree a)
convertMethodAndPathToResourceTree reqMethod p [r] = 
  if (comparePathToResourceTree p r && methodInResource reqMethod r)
    then Just r
    else Nothing 
convertMethodAndPathToResourceTree reqMethod p (r:rs) = 
  if (comparePathToResourceTree p r && methodInResource reqMethod r)
    then Just r
    else convertMethodAndPathToResourceTree reqMethod p rs
convertMethodAndPathToResourceTree _ _ _ = Nothing

methodInResource :: String -> ResourceTree a -> Bool
methodInResource reqMethod (ResourceLeaf rl) = reqMethod `elem` (methodsMethods . resourceDispatch $ rl)
methodInResource _ _ = False
  
comparePathToResourceTree :: [String] -> ResourceTree a -> Bool
comparePathToResourceTree ts (ResourceLeaf rl) = compareStringsToPieces ts (resourcePieces rl)
-- I haven't seen any examples of ResourceParent so I am ignoring it for now
--comparePathToResourceTree t (ResourceParent s co ps rts)
comparePathToResourceTree _ _ = False

-- the list of strings and pieces 
compareStringsToPieces :: [String] -> [Piece a] -> Bool
compareStringsToPieces [t] [p] = compareStringToPiece t p
compareStringsToPieces tss@(t:ts) pss@(p:ps) = 
  if length tss == length pss 
    then compareStringToPiece t p && compareStringsToPieces ts ps
    else False
compareStringsToPieces [] [] = True    
compareStringsToPieces _ _ = False


compareStringToPiece :: String -> Piece a -> Bool
compareStringToPiece t (Static s) = t == s

-- the dynamic type is stored as a string, we would have to use template haskell
-- to make it a type and check if `_t` can read as the type in `_d`. This would 
-- be useful but ignoring it for the moment.
compareStringToPiece _t (Dynamic _d) = True 


{-# LANGUAGE PatternGuards #-}

module Yesod.Routes.Metrics where 

import Data.Char (isUpper, isLower, isSpace, toLower)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (concat, foldl', isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Network.Wai
import Yesod.Routes.TH.Types

getResource :: Request -> [ResourceTree a] -> Maybe String
getResource req resourceTrees =
  case mResourceTree of
    Nothing -> Nothing
    Just rt -> getRouteName (BS.unpack $ requestMethod req) rt
  where
    mResourceTree = getRequestResourceTree req resourceTrees
    

getRouteName :: String -> ResourceTree a -> Maybe String
getRouteName method rt@(ResourceLeaf rl) =
  if methodInResource method rt
    then Just $ (toLower <$> method) ++ (resourceName rl)
    else Nothing
getRouteName _ _ = Nothing

getRequestResourceTree :: Request -> [ResourceTree a] -> Maybe (ResourceTree a)
getRequestResourceTree req resourceTrees = getRequestResourceTree' reqMethod reqPaths resourceTrees
  where 
    reqMethod = BS.unpack $ requestMethod req
    reqPaths  = T.unpack <$> pathInfo req

-- list of resource tree 
-- find first that matches, otherwise return nothing
getRequestResourceTree' :: String -> [String] -> [ResourceTree a] -> Maybe (ResourceTree a)
getRequestResourceTree' reqMethod p [r] = 
  if (compareTextToResourceTree p r && methodInResource reqMethod r)
    then Just r
    else Nothing 
getRequestResourceTree' reqMethod p (r:rs) = 
  if (compareTextToResourceTree p r && methodInResource reqMethod r)
    then Just r
    else getRequestResourceTree' reqMethod p rs
getRequestResourceTree' _ _ _ = Nothing

methodInResource :: String -> ResourceTree a -> Bool
methodInResource reqMethod (ResourceLeaf rl) = reqMethod `elem` (methodsMethods . resourceDispatch $ rl)
methodInResource _ _ = False
  
compareTextToResourceTree :: [String] -> ResourceTree a -> Bool
compareTextToResourceTree ts (ResourceLeaf rl) = comparePieces ts (resourcePieces rl)
-- I haven't seen any examples of ResourceParent so I am ignoring it for now
--compareTextToResourceTree t (ResourceParent s co ps rts)
compareTextToResourceTree _ _ = False

comparePieces :: [String] -> [Piece a] -> Bool
comparePieces [t]    [p]    = comparePiece t p
comparePieces tss@(t:ts) pss@(p:ps) = 
  if length tss == length pss 
    then comparePiece t p && comparePieces ts ps
    else False
comparePieces [] [] = True    
comparePieces _ _ = False

comparePiece :: String -> Piece a -> Bool
comparePiece t (Static s) = t == s

-- the dynamic type is stored as a string, we would have to use template haskell
-- to make it a type and check if `_t` can read as the type in `_d`. This would 
-- be useful but ignoring it for the moment.
comparePiece _t (Dynamic _d) = True 

showResourceTree :: (Show a) => ResourceTree a -> String
showResourceTree (ResourceLeaf rl)              = 
  "ResourceLeaf: " ++ show rl
showResourceTree (ResourceParent s co ps rts) = 
  "ResourceParent: " ++ (show s) ++ ", CheckOverlap: " ++ (show co) ++ (concat $ showPiece <$> ps) ++ "\n" ++ (concat $ showResourceTree <$> rts)
  where 
    showPiece p = 
      ", " ++ show p
      
-- foldl (&&) True ((> 0) <$> [1,1])

t :: [a] -> String
t []      = "none"
t [x]     = "one"
t (x:_xs) = "more than one"

{-
"ResourceLeaf: Resource {resourceName = \"HomeR\", resourcePieces = [], resourceDispatch = Methods {methodsMulti = Nothing, methodsMethods = [\"GET\"]}, resourceAttrs = [], resourceCheck = True}"
"ResourceLeaf: Resource {resourceName = \"BlogR\", resourcePieces = [Static \"blog\"], resourceDispatch = Methods {methodsMulti = Nothing, methodsMethods = [\"GET\",\"POST\"]}, resourceAttrs = [], resourceCheck = True}"
"ResourceLeaf: Resource {resourceName = \"BlogPostR\", resourcePieces = [Static \"blog\",Dynamic \"BlogId\"], resourceDispatch = Methods {methodsMulti = Nothing, methodsMethods = [\"GET\",\"POST\"]}, resourceAttrs = [], resourceCheck = True}"
"ResourceLeaf: Resource {resourceName = \"StaticR\", resourcePieces = [Static \"static\"], resourceDispatch = Subsite {subsiteType = \"Static\", subsiteFunc = \"getStatic\"}, resourceAttrs = [], resourceCheck = True}"

-}


-- | Converts a multi-line string to a set of resources. See documentation for
-- the format of this string. This is a partial function which calls 'error' on
-- invalid input.
resourcesFromString :: String -> [ResourceTree String]
resourcesFromString =
    fst . parse 0 . filter (not . all (== ' ')) . lines . filter (/= '\r')
  where
    parse _ [] = ([], [])
    parse indent (thisLine:otherLines)
        | length spaces < indent = ([], thisLine : otherLines)
        | otherwise = (this others, remainder)
      where
        parseAttr ('!':x) = Just x
        parseAttr _ = Nothing

        stripColonLast =
            go id
          where
            go _ [] = Nothing
            go front [x]
                | null x = Nothing
                | last x == ':' = Just $ front [init x]
                | otherwise = Nothing
            go front (x:xs) = go (front . (x:)) xs

        spaces = takeWhile (== ' ') thisLine
        (others, remainder) = parse indent otherLines'
        (this, otherLines') =
            case takeWhile (not . isPrefixOf "--") $ splitSpaces thisLine of
                (pattern:rest0)
                    | Just (constr:rest) <- stripColonLast rest0
                    , Just attrs <- mapM parseAttr rest ->
                    let (children, otherLines'') = parse (length spaces + 1) otherLines
                        children' = addAttrs attrs children
                        (pieces, Nothing, check) = piecesFromStringCheck pattern
                     in ((ResourceParent constr check pieces children' :), otherLines'')
                (pattern:constr:rest) ->
                    let (pieces, mmulti, check) = piecesFromStringCheck pattern
                        (attrs, rest') = takeAttrs rest
                        disp = dispatchFromString rest' mmulti
                     in ((ResourceLeaf (Resource constr pieces disp attrs check):), otherLines)
                [] -> (id, otherLines)
                _ -> error $ "Invalid resource line: " ++ thisLine

-- | Splits a string by spaces, as long as the spaces are not enclosed by curly brackets (not recursive).
splitSpaces :: String -> [String]
splitSpaces "" = []
splitSpaces str = 
    let (rest, piece) = parse $ dropWhile isSpace str in
    piece:(splitSpaces rest)

    where 
        parse :: String -> ( String, String)
        parse ('{':s) = fmap ('{':) $ parseBracket s
        parse (c:s) | isSpace c = (s, [])
        parse (c:s) = fmap (c:) $ parse s
        parse "" = ("", "")

        parseBracket :: String -> ( String, String)
        parseBracket ('{':_) = error $ "Invalid resource line (nested curly bracket): " ++ str
        parseBracket ('}':s) = fmap ('}':) $ parse s
        parseBracket (c:s) = fmap (c:) $ parseBracket s
        parseBracket "" = error $ "Invalid resource line (unclosed curly bracket): " ++ str

piecesFromStringCheck :: String -> ([Piece String], Maybe String, Bool)
piecesFromStringCheck s0 =
    (pieces, mmulti, check)
  where
    (s1, check1) = stripBang s0
    (pieces', mmulti') = piecesFromString $ drop1Slash s1
    pieces = map snd pieces'
    mmulti = fmap snd mmulti'
    check = check1 && all fst pieces' && maybe True fst mmulti'

    stripBang ('!':rest) = (rest, False)
    stripBang x = (x, True)

addAttrs :: [String] -> [ResourceTree String] -> [ResourceTree String]
addAttrs attrs =
    map goTree
  where
    goTree (ResourceLeaf res) = ResourceLeaf (goRes res)
    goTree (ResourceParent w x y z) = ResourceParent w x y (map goTree z)

    goRes res =
        res { resourceAttrs = noDupes ++ resourceAttrs res }
      where
        usedKeys = Set.fromList $ map fst $ mapMaybe toPair $ resourceAttrs res
        used attr =
            case toPair attr of
                Nothing -> False
                Just (key, _) -> key `Set.member` usedKeys
        noDupes = filter (not . used) attrs

    toPair s =
        case break (== '=') s of
            (x, '=':y) -> Just (x, y)
            _ -> Nothing

-- | Take attributes out of the list and put them in the first slot in the
-- result tuple.
takeAttrs :: [String] -> ([String], [String])
takeAttrs =
    go id id
  where
    go x y [] = (x [], y [])
    go x y (('!':attr):rest) = go (x . (attr:)) y rest
    go x y (z:rest) = go x (y . (z:)) rest

drop1Slash :: String -> String
drop1Slash ('/':x) = x
drop1Slash x = x

dispatchFromString :: [String] -> Maybe String -> Dispatch String
dispatchFromString rest mmulti
    | null rest = Methods mmulti []
    | all (all isUpper) rest = Methods mmulti rest
dispatchFromString [subTyp, subFun] Nothing =
    Subsite subTyp subFun
dispatchFromString [_, _] Just{} =
    error "Subsites cannot have a multipiece"
dispatchFromString rest _ = error $ "Invalid list of methods: " ++ show rest

piecesFromString :: String -> ([(CheckOverlap, Piece String)], Maybe (CheckOverlap, String))
piecesFromString "" = ([], Nothing)
piecesFromString x =
    case (this, rest) of
        (Left typ, ([], Nothing)) -> ([], Just typ)
        (Left _, _) -> error "Multipiece must be last piece"
        (Right piece, (pieces, mtyp)) -> (piece:pieces, mtyp)
  where
    (y, z) = break (== '/') x
    this = pieceFromString y
    rest = piecesFromString $ drop 1 z

pieceFromString :: String -> Either (CheckOverlap, String) (CheckOverlap, Piece String)
pieceFromString ('#':'!':x) = Right $ (False, Dynamic $ dropBracket x)
pieceFromString ('!':'#':x) = Right $ (False, Dynamic $ dropBracket x) -- https://github.com/yesodweb/yesod/issues/652
pieceFromString ('#':x) = Right $ (True, Dynamic $ dropBracket x)

pieceFromString ('*':'!':x) = Left (False, x)
pieceFromString ('+':'!':x) = Left (False, x)

pieceFromString ('!':'*':x) = Left (False, x)
pieceFromString ('!':'+':x) = Left (False, x)

pieceFromString ('*':x) = Left (True, x)
pieceFromString ('+':x) = Left (True, x)

pieceFromString ('!':x) = Right $ (False, Static x)
pieceFromString x = Right $ (True, Static x)

dropBracket :: String -> String
dropBracket str@('{':x) = case break (== '}') x of
    (s, "}") -> s
    _ -> error $ "Unclosed bracket ('{'): " ++ str
dropBracket x = x

{-
parseType :: String -> Type
parseType orig =
    maybe (error $ "Invalid type: " ++ show orig) ttToType $ parseTypeTree orig
-}
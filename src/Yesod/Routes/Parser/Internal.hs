module Yesod.Routes.Parser.Internal where

import           Data.Char (isUpper, isSpace)
import           Data.List (isPrefixOf)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Yesod.Routes.TH.Types


-- This code copied from Yesod.Routes.TH.Types in yesod-core. The parsing code 
-- is not exposed but we want the same parser for the routes file.
  
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

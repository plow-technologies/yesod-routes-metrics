{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import qualified Data.Vault.Lazy as V
import qualified Network.HTTP.Types           as H
import Network.Socket
import Network.Wai.Internal
import Yesod.Routes.Metrics
import Yesod.Routes.TH.Types
import Test.Hspec

defaultRequest :: Request
defaultRequest = 
  Request 
    "GET" 
    H.http11 
    ""
    ""
    []
    False
    (SockAddrUnix "")
    []
    []
    (return "")
    V.empty
    (KnownLength 0)
    Nothing
    Nothing
    Nothing
    Nothing

deriving instance Eq (Dispatch String)
deriving instance Eq (Resource String)
deriving instance Eq (Piece String)
deriving instance Show (ResourceTree String)
deriving instance Eq (ResourceTree String)
{-
instance (Show a) => Show (ResourceTree a) where
  show (ResourceLeaf rl) =  "ResourceLeaf: " ++ show rl
  show (ResourceParent s co ps rts) = 
    "ResourceParent: " ++ (show s) ++ ", CheckOverlap: " ++ (show co) ++ (concat $ showPiece <$> ps) ++ "\n" ++ (concat $ showResourceTree <$> rts)
    where showPiece p = ", " ++ show p
-}

homeR :: ResourceTree String
homeR = ResourceLeaf $ Resource "HomeR" [] (Methods Nothing ["GET"]) [] False

testR :: ResourceTree String
testR = ResourceLeaf $ Resource "TestR" [Static "test"] (Methods Nothing ["GET"]) [] False

newGroupR :: ResourceTree String
newGroupR = ResourceLeaf $ Resource "NewGroupR" [Static "group"] (Methods Nothing ["POST"]) [] False

groupR :: ResourceTree String
groupR = ResourceLeaf $ Resource "GroupR" [Static "group", Dynamic "GroupId"] (Methods Nothing ["GET"]) [] False

newUserR :: ResourceTree String
newUserR = ResourceLeaf $ Resource "NewUserR" [Static "group", Dynamic "GroupId", Static "user"] (Methods Nothing ["POST"]) [] False

userR :: ResourceTree String
userR = ResourceLeaf $ Resource "UserR" [Static "group", Dynamic "GroupId", Static "user", Dynamic "UserId"] (Methods Nothing ["GET"]) [] False

appRoutes :: [ResourceTree String]
appRoutes = [homeR,testR,newGroupR,groupR,newUserR,userR]

spec :: Spec
spec = do
  describe "compareTextToResourceTree" $ do 
    it "path [] matches homeR" $
      (compareTextToResourceTree [] homeR) `shouldBe` True
    
    it "path [\"/\"] does not match homeR" $
      (compareTextToResourceTree ["/"] homeR) `shouldBe` False
    
    it "path [\"test\"] matches testR" $
      (compareTextToResourceTree ["test"] testR) `shouldBe` True
    
    it "path [\"test\",\"/\"] does not match testR" $
      (compareTextToResourceTree ["test","/"] testR) `shouldBe` False
    
    it "path [\"group\", \"1\"] matches groupR" $
      (compareTextToResourceTree ["group", "1"] groupR) `shouldBe` True
    
    it "path [\"group\"] does not match groupR" $
      (compareTextToResourceTree ["group"] groupR) `shouldBe` False
    
    it "path [\"group\",\"1\",\"user\",\"1\"] matches userR" $
      (compareTextToResourceTree ["group","1","user","1"] userR) `shouldBe` True
    
    it "path [\"test\",\"/\"] does not match userR" $
      (compareTextToResourceTree ["user","1"] userR) `shouldBe` False
  
  describe "getRequestResourceTree'" $ do
    it "GET path [] returns homeR" $ do 
      (getRequestResourceTree' "GET" [] appRoutes) `shouldBe` (Just homeR)
      (getRequestResourceTree' "GET" ["/"] appRoutes) `shouldBe` Nothing
      
    it "POST path [\"group\"] returns newGroupR" $
      (getRequestResourceTree' "POST" ["group"] appRoutes) `shouldBe` (Just newGroupR)

    it "GET path [\"group\",\"1\"] returns groupR" $ do 
      (getRequestResourceTree' "GET" ["group","1"] appRoutes) `shouldBe` (Just groupR)
      (getRequestResourceTree' "GET" ["group"] appRoutes) `shouldBe` Nothing
      
    it "POST path [\"group\",\"1\",\"user\"] returns newUserR" $ do 
      (getRequestResourceTree' "POST" ["group","1","user"] appRoutes) `shouldBe` (Just newUserR)
      (getRequestResourceTree' "POST" ["group","1"] appRoutes) `shouldBe` Nothing
      
    it "GET path [\"group\",\"1\",\"user\",\"1\"] returns userR" $ do
      (getRequestResourceTree' "GET" ["group","1","user","1"] appRoutes) `shouldBe` (Just userR)
      (getRequestResourceTree' "GET" ["group","1","user"] appRoutes) `shouldBe` Nothing  

  describe "getRequestResourceTree" $ do
    it "GET path [] returns homeR" $ do 
      (getRequestResourceTree (defaultRequest {requestMethod = "GET", pathInfo = []}) appRoutes) `shouldBe` (Just homeR)

    it "POST path [\"group\"] returns newGroupR" $
      (getRequestResourceTree (defaultRequest {requestMethod = "POST", pathInfo = ["group"]}) appRoutes) `shouldBe` (Just newGroupR)

  describe "getResource" $ do
    it "GET path [] returns getHomeR" $ do 
      (getResource (defaultRequest {requestMethod = "GET", pathInfo = []}) appRoutes) `shouldBe` (Just "getHomeR")

    it "POST path [\"group\"] returns newGroupR" $
      (getResource (defaultRequest {requestMethod = "POST", pathInfo = ["group"]}) appRoutes) `shouldBe` (Just "postNewGroupR")


{- Wai Request

"Request {requestMethod = \"GET\"
         , httpVersion = HTTP/1.1
         , rawPathInfo = \"/test\"
         , rawQueryString = \"\"
         , requestHeaders = [(\"Host\",\"localhost:3000\")
         ,(\"User-Agent\",\"curl/7.47.0\"),(\"Accept\",\"*/*\")]
         , isSecure = False
         , remoteHost = 127.0.0.1:58104
         , pathInfo = [\"test\"]
         , queryString = []
         , requestBody = <IO ByteString>
         , vault = <Vault>
         , requestBodyLength = KnownLength 0
         , requestHeaderHost = Just \"localhost:3000\"
         , requestHeaderRange = Nothing}"

-}

{-
when path is "/"
ResourceLeaf: 
  Resource {resourceName = \"HomeR\"
           , resourcePieces = []
           , resourceDispatch = Methods {methodsMulti = Nothing
                                        , methodsMethods = [\"GET\"]}
                                        , resourceAttrs = []
                                        , resourceCheck = True}"
-}

main :: IO ()
main = hspec spec
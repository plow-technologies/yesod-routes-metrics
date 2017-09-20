{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           App (runYesodServer)

import           Control.Concurrent (forkIO, killThread, threadDelay)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vault.Lazy as V

import           Network.HTTP (simpleHTTP, getRequest)
import qualified Network.HTTP.Types           as H
import           Network.Socket
import           Network.Wai.Internal

import           System.Metrics (Value(..), newStore, sampleAll)

import           Test.Hspec

import           Yesod.Routes.Metrics
import           Yesod.Routes.Convert.Internal
import           Yesod.Routes.Parser.Internal
import           Yesod.Routes.TH.Types


-- only 
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
  describe "comparePathToResourceTree" $ do 
    it "path [] matches homeR" $
      (comparePathToResourceTree [] homeR) `shouldBe` True
    
    it "path [\"/\"] does not match homeR" $
      (comparePathToResourceTree ["/"] homeR) `shouldBe` False
    
    it "path [\"test\"] matches testR" $
      (comparePathToResourceTree ["test"] testR) `shouldBe` True
    
    it "path [\"test\",\"/\"] does not match testR" $
      (comparePathToResourceTree ["test","/"] testR) `shouldBe` False
    
    it "path [\"group\", \"1\"] matches groupR" $
      (comparePathToResourceTree ["group", "1"] groupR) `shouldBe` True
    
    it "path [\"group\"] does not match groupR" $
      (comparePathToResourceTree ["group"] groupR) `shouldBe` False
    
    it "path [\"group\",\"1\",\"user\",\"1\"] matches userR" $
      (comparePathToResourceTree ["group","1","user","1"] userR) `shouldBe` True
    
    it "path [\"test\",\"/\"] does not match userR" $
      (comparePathToResourceTree ["user","1"] userR) `shouldBe` False
  
  describe "convertMethodAndPathToResourceTree" $ do
    it "GET path [] returns homeR" $ do 
      (convertMethodAndPathToResourceTree "GET" [] appRoutes) `shouldBe` (Just homeR)
      (convertMethodAndPathToResourceTree "GET" ["/"] appRoutes) `shouldBe` Nothing
      
    it "POST path [\"group\"] returns newGroupR" $
      (convertMethodAndPathToResourceTree "POST" ["group"] appRoutes) `shouldBe` (Just newGroupR)

    it "GET path [\"group\",\"1\"] returns groupR" $ do 
      (convertMethodAndPathToResourceTree "GET" ["group","1"] appRoutes) `shouldBe` (Just groupR)
      (convertMethodAndPathToResourceTree "GET" ["group"] appRoutes) `shouldBe` Nothing
      
    it "POST path [\"group\",\"1\",\"user\"] returns newUserR" $ do 
      (convertMethodAndPathToResourceTree "POST" ["group","1","user"] appRoutes) `shouldBe` (Just newUserR)
      (convertMethodAndPathToResourceTree "POST" ["group","1"] appRoutes) `shouldBe` Nothing
      
    it "GET path [\"group\",\"1\",\"user\",\"1\"] returns userR" $ do
      (convertMethodAndPathToResourceTree "GET" ["group","1","user","1"] appRoutes) `shouldBe` (Just userR)
      (convertMethodAndPathToResourceTree "GET" ["group","1","user"] appRoutes) `shouldBe` Nothing  

  describe "convertRequestToResourceTree" $ do
    it "GET path [] returns homeR" $ do 
      (convertRequestToResourceTree (defaultRequest {requestMethod = "GET", pathInfo = []}) appRoutes) `shouldBe` (Just homeR)

    it "POST path [\"group\"] returns newGroupR" $
      (convertRequestToResourceTree (defaultRequest {requestMethod = "POST", pathInfo = ["group"]}) appRoutes) `shouldBe` (Just newGroupR)

  describe "convertRequestToRouteName" $ do
    it "GET path [] returns getHomeR" $ do 
      (convertRequestToRouteName (defaultRequest {requestMethod = "GET", pathInfo = []}) appRoutes) `shouldBe` (Just "getHomeR")

    it "POST path [\"group\"] returns newGroupR" $
      (convertRequestToRouteName (defaultRequest {requestMethod = "POST", pathInfo = ["group"]}) appRoutes) `shouldBe` (Just "postNewGroupR")
  
  describe "convertResourceTreesToRouteNames" $ do
    it "convertResourceTreesToRouteNames appRoutes should contain a list of route names" $
      convertResourceTreesToRouteNames appRoutes `shouldContain`
        [ "getHomeR"
        , "getTestR"
        , "postNewGroupR"
        , "getGroupR"
        , "postNewUserR"
        , "getUserR"
        ]
        
  describe "registerYesodMetricsWithResourceTrees" $ do
    it "makes underlined response status when underlined is True" $ do
      store <- newStore
      routes <- registerYesodMetricsWithResourceTrees defaultYesodMetricsConfig appRoutes store
      (Map.keys $ routeCounters routes) `shouldContain`
        [ "getHomeR"
        , "getHomeR_response_status_1xx"
        , "getHomeR_response_status_2xx"
        , "getHomeR_response_status_3xx"
        , "getHomeR_response_status_4xx"
        , "getHomeR_response_status_5xx"
        ]
        
    it "do not create stores for each response when verbose is False" $ do
      store <- newStore
      routes <- registerYesodMetricsWithResourceTrees (defaultYesodMetricsConfig {verbose = False}) appRoutes store
      (Map.keys $ routeCounters routes) `shouldMatchList`
        [ "getHomeR"
        , "getTestR"
        , "postNewGroupR"
        , "getGroupR"
        , "postNewUserR"
        , "getUserR"
        ]
        
    it "makes spaced response status when underlined is False" $ do
      store <- newStore
      routes <- registerYesodMetricsWithResourceTrees (defaultYesodMetricsConfig { underlined = False }) appRoutes store
      (Map.keys $ routeCounters routes) `shouldContain`
        [ "getHomeR"
        , "getHomeR response status 1xx"
        , "getHomeR response status 2xx"
        , "getHomeR response status 3xx"
        , "getHomeR response status 4xx"
        , "getHomeR response status 5xx"
        ]      

    it "makes spaced routes and response status when underlined is False and using addSpacesToRoute" $ do
      store <- newStore
      routes <- registerYesodMetricsWithResourceTrees (defaultYesodMetricsConfig { underlined = False, alterRouteName = addSpacesToRoute }) appRoutes store
      (Map.keys $ routeCounters routes) `shouldContain`
        [ "get Home R"
        , "get Home R response status 1xx"
        , "get Home R response status 2xx"
        , "get Home R response status 3xx"
        , "get Home R response status 4xx"
        , "get Home R response status 5xx"
        ]
  
  describe "Run a test server and make sure route metrics are stored in the right places" $ do
    it "spacedYesodMetricsConfig should add spaces to route names in the store" $ do 
      store <- newStore
      threadId <- forkIO $ runYesodServer 3333 spacedYesodMetricsConfig store
      threadDelay 2000000
      simpleHTTP (getRequest "http://127.0.0.1:3333")
      sample <- sampleAll store
      
      HM.lookup "total requests per interval" sample `shouldBe` (Just $ Counter 1)
      HM.lookup "get Home R" sample `shouldBe` (Just $ Counter 1)
      HM.lookup "get Home R response status 2xx" sample `shouldBe` (Just $ Counter 1)
      HM.lookup "getHomeR" sample `shouldBe` Nothing
      HM.lookup "getHomeR_response_status_2xx" sample `shouldBe` Nothing
      HM.lookup "post New User R" sample `shouldBe` (Just $ Counter 0)

      HM.lookup "post New User R max latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "post New User R min latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "post New User R avg latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "post New User R 50th percentile latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "post New User R 75th percentile latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "post New User R 90th percentile latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "post New User R 99th percentile latency" sample `shouldBe` (Just $ Gauge 0)
      
      -- test latency data expenctations

      simpleHTTP (getRequest "http://127.0.0.1:3333/delay/1000/10000")
      simpleHTTP (getRequest "http://127.0.0.1:3333/delay/1000/10000")
      simpleHTTP (getRequest "http://127.0.0.1:3333/delay/1000/10000")
      simpleHTTP (getRequest "http://127.0.0.1:3333/delay/1000/10000")
      simpleHTTP (getRequest "http://127.0.0.1:3333/delay/1000/10000")
      
      sample2 <- sampleAll store

      HM.lookup "total requests per interval" sample2 `shouldBe` (Just $ Counter 6)
      HM.lookup "get Delay R" sample2 `shouldBe` (Just $ Counter 5)

      let (Just (Gauge minLatency)) = HM.lookup "get Delay R min latency" sample2
      let (Just (Gauge maxLatency)) = HM.lookup "get Delay R max latency" sample2
      let (Just (Gauge avgLatency)) = HM.lookup "get Delay R avg latency" sample2
      let (Just (Gauge p50Latency)) = HM.lookup "get Delay R 50th percentile latency" sample2
      let (Just (Gauge p75Latency)) = HM.lookup "get Delay R 75th percentile latency" sample2
      let (Just (Gauge p90Latency)) = HM.lookup "get Delay R 90th percentile latency" sample2
      let (Just (Gauge p99Latency)) = HM.lookup "get Delay R 99th percentile latency" sample2
      
      minLatency `shouldSatisfy` (> 0)
      avgLatency `shouldSatisfy` (> 0)      
      maxLatency `shouldSatisfy` (> 0)
      (minLatency <= maxLatency) `shouldBe` True
      (minLatency <= avgLatency && avgLatency <= maxLatency) `shouldBe` True
      (    minLatency <= p50Latency
        && p50Latency <= p75Latency 
        && p75Latency <= p90Latency 
        && p90Latency <= p99Latency
        && p99Latency <= maxLatency
           ) `shouldBe` True
      
      -- force longer delay, all latency values should increase
      simpleHTTP (getRequest "http://127.0.0.1:3333/delay/11000/20000")
      simpleHTTP (getRequest "http://127.0.0.1:3333/delay/11000/20000")
      simpleHTTP (getRequest "http://127.0.0.1:3333/delay/11000/20000")
      simpleHTTP (getRequest "http://127.0.0.1:3333/delay/11000/20000")
      simpleHTTP (getRequest "http://127.0.0.1:3333/delay/11000/20000")

      sample3 <- sampleAll store

      let (Just (Gauge minLatency2)) = HM.lookup "get Delay R min latency" sample3
      let (Just (Gauge maxLatency2)) = HM.lookup "get Delay R max latency" sample3
      let (Just (Gauge avgLatency2)) = HM.lookup "get Delay R avg latency" sample3
      let (Just (Gauge p50Latency2)) = HM.lookup "get Delay R 50th percentile latency" sample3
      let (Just (Gauge p75Latency2)) = HM.lookup "get Delay R 75th percentile latency" sample3
      let (Just (Gauge p90Latency2)) = HM.lookup "get Delay R 90th percentile latency" sample3
      let (Just (Gauge p99Latency2)) = HM.lookup "get Delay R 99th percentile latency" sample3

      (minLatency <= minLatency2) `shouldBe` True
      (maxLatency < maxLatency2)  `shouldBe` True
      (avgLatency < avgLatency2)  `shouldBe` True
      (p50Latency < p50Latency2)  `shouldBe` True
      (p75Latency < p75Latency2)  `shouldBe` True
      (p90Latency < p90Latency2)  `shouldBe` True
      (p99Latency < p99Latency2)  `shouldBe` True

      killThread threadId

    it "defaultYesodMetricsConfig does not alter the route names from Yesod and adds response statuses with underlines" $ do 
      store <- newStore
      threadId <- forkIO $ runYesodServer 3334 defaultYesodMetricsConfig store
      threadDelay 2000000
      simpleHTTP (getRequest "http://127.0.0.1:3334")
      sample <- sampleAll store

      HM.lookup "total_requests_per_interval" sample `shouldBe` (Just $ Counter 1)
      HM.lookup "getHomeR" sample `shouldBe` (Just $ Counter 1)
      HM.lookup "getHomeR_response_status_2xx" sample `shouldBe` (Just $ Counter 1)
      HM.lookup "get Home R" sample `shouldBe` Nothing
      HM.lookup "get Home R response status 2xx" sample `shouldBe` Nothing
      HM.lookup "postNewUserR" sample `shouldBe` (Just $ Counter 0)

      HM.lookup "postNewUserR_max_latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "postNewUserR_min_latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "postNewUserR_avg_latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "postNewUserR_50th_percentile_latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "postNewUserR_75th_percentile_latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "postNewUserR_90th_percentile_latency" sample `shouldBe` (Just $ Gauge 0)
      HM.lookup "postNewUserR_99th_percentile_latency" sample `shouldBe` (Just $ Gauge 0)

      
      killThread threadId


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
Resource { resourceName = \"HomeR\"
         , resourcePieces = []
         , resourceDispatch = 
             Methods { methodsMulti = Nothing
                     , methodsMethods = [\"GET\"]}
                     , resourceAttrs = []
                     , resourceCheck = True}"
-}

main :: IO ()
main = hspec spec
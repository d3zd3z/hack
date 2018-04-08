{-# LANGUAGE OverloadedStrings #-}

-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Test.Tasty
import Test.Tasty.HUnit

import Hack.Weave.Header (Header)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
   [ {- testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT, -}
      testCase "Header Test" testHeader ]

testHeader :: IO ()
testHeader = do
   let lt1 = L.fromStrict t1
   enc <- maybe (assertFailure "Decode failure") return $
      ((A.decode lt1) :: Maybe Header)
   let dec = A.encode enc
   putStrLn $ show enc
   putStrLn $ show dec
   lt1 @=? dec

t1 :: ByteString
t1 = "{\"version\":1,\"deltas\":[{\"name\":\"2018-04-06T14:38:47.973363784-06:00\",\"number\":1,\"tags\":{\"dir\":\"/home/davidb/back/hack\"},\"time\":\"2018-04-06T20:38:48.189712199Z\"}]}"

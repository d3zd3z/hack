-- Test suite

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as L
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Weave.Header (Header)
import Sure.HashTest
import Weave.Sccs

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "Header Test" testHeader,
        testCase "Sccs Many Delta Test" tryManyDeltas,
        testCase "hashtest" testHash,
        testCase "toFile" testToFile ]

testHeader :: IO ()
testHeader = do
    let lt1 = L.fromStrict t1
    enc <- maybe (assertFailure "Decode failure") return $
        ((A.decode lt1) :: Maybe Header)
    let dec = A.encode enc
    lt1 @=? dec

t1 :: ByteString
t1 = "{\"version\":1,\"deltas\":[{\"name\":\"2018-04-06T14:38:47.973363784-06:00\",\"number\":1,\"tags\":{\"dir\":\"/home/davidb/back/hack\"},\"time\":\"2018-04-06T20:38:48.189712199Z\"}]}"

tryManyDeltas :: IO ()
tryManyDeltas = do
    have <- haveSccs
    if have then runManyDeltas else return ()

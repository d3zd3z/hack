-- |Weave writing.
--
-- Take the weave data given, and write it to a delta file.

{-# LANGUAGE OverloadedStrings #-}

module Hack.Weave.Write (
   weaveEncode
) where

import Data.Aeson (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import Hack.Weave.Types (WeaveElement(..))

weaveEncode :: InputStream WeaveElement -> OutputStream B.ByteString -> IO ()
weaveEncode source sink = do
   toLines <- Streams.unlines sink
   let
      gen = do
         elt <- Streams.read source
         case elt of
            Nothing -> Streams.write Nothing toLines
            Just w -> do
               Streams.write (Just $ encodeOne w) toLines
               gen
   gen

encodeOne :: WeaveElement -> B.ByteString
encodeOne (WeaveHeader hd) = B.concat ["\^At", LB.toStrict $ encode hd]
encodeOne (WeaveInsert line) = B.concat ["\^AI ", C8.pack $ show line]
encodeOne (WeaveDelete line) = B.concat ["\^AD ", C8.pack $ show line]
encodeOne (WeaveEnd line) = B.concat ["\^AE ", C8.pack $ show line]
encodeOne (WeaveError line) = line
encodeOne (WeavePlain text _) = text

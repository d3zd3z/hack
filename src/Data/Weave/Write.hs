-- |Weave writing
--
-- Take the weave data given, and write it to a delta file.

{-# LANGUAGE OverloadedStrings #-}

module Data.Weave.Write (
    weaveEncode,
    firstDelta
) where

import Conduit
import Data.Aeson (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB

import Data.Weave.Header
import Data.Weave.Types (WeaveElement(..))

-- |Encode a weave stream as a sequence of lines.
weaveEncode :: Monad m => ConduitT WeaveElement B.ByteString m ()
weaveEncode = mapC encodeOne

encodeOne :: WeaveElement -> B.ByteString
encodeOne (WeaveHeader hd) = B.concat ["\^At", LB.toStrict $ encode hd]
encodeOne (WeaveInsert line) = B.concat ["\^AI ", C8.pack $ show line]
encodeOne (WeaveDelete line) = B.concat ["\^AD ", C8.pack $ show line]
encodeOne (WeaveEnd line) = B.concat ["\^AE ", C8.pack $ show line]
encodeOne (WeaveError line) = line
encodeOne (WeavePlain text _) = text

-- |Map from ByteStrings, which are the lines of payload, and encode
-- them as the initial delta.
firstDelta :: Monad m => Header -> ConduitT B.ByteString WeaveElement m ()
firstDelta hdr = do
    yield $ WeaveHeader hdr
    yield $ WeaveInsert 1
    awaitForever $ \p -> yield $ WeavePlain p Nothing
    yield $ WeaveEnd 1

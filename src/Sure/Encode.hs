-- | Encode a suretree in sure format

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sure.Encode (
   treeEncode,
   sureEncoder
) where

import Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified Data.Vector as V

import Sure.Types

-- | Linearize an encoding of a SureTree into a '[B.ByteString]' that
-- can by re-decoded back to the tree later.
treeEncode :: SureTree -> [B.ByteString]
treeEncode tree = "asure-2.0" : "-----" : treeEncode' tree

treeEncode' :: SureTree -> [B.ByteString]
treeEncode' SureTree{..} =
   oneNode 'd' stName stAtts :
   concatMap treeEncode' (V.toList stChildren) ++
   ["-"] ++
   (map fileEncode $ V.toList stFiles) ++
   ["u"]

oneNode :: Char -> B.ByteString -> AttMap -> B.ByteString
oneNode key name atts = L.toStrict $ B.toLazyByteString encode
   where
      encode = B.char7 key <> escape name <> B.char7 ' ' <>
         encodeAtts atts

sureEncoder :: Monad m => ConduitT SureNode B.ByteString m ()
sureEncoder = do
    yield "asure-2.0"
    yield "-----"
    awaitForever $ yield . L.toStrict . B.toLazyByteString . encodeSureNode

encodeSureNode :: SureNode -> B.Builder
encodeSureNode (SureEnter name atts) = encodeNode 'd' name atts
encodeSureNode SureLeave             = B.char7 'u'
encodeSureNode SureSep               = B.char7 '-'
encodeSureNode (SureNode name atts)  = encodeNode 'f' name atts

encodeNode :: Char -> B.ByteString -> AttMap -> B.Builder
encodeNode key name atts = B.char7 key <>
    escape name <> B.char7 ' ' <>
    encodeAtts atts

fileEncode :: SureFile -> B.ByteString
fileEncode SureFile{..} = oneNode 'f' sfName sfAtts

encodeAtts :: AttMap -> B.Builder
encodeAtts atts = B.char7 '[' <> mconcat [oneAtt att | att <- Map.toList atts] <> B.char7 ']'
   where
      oneAtt (key, value) = B.byteString key <> B.char7 ' ' <>
         escape value <> B.char7 ' '

escape :: B.ByteString -> B.Builder
escape cs = C8.foldl' (\acc ch -> acc <> esc ch) mempty cs
   where
      esc :: Char -> B.Builder
      esc c
         | '!' <= c && c <= '~' && c /= '=' = B.char7 c
         | otherwise = B.char7 '=' <> (B.word8HexFixed $ fromIntegral $ ord c)

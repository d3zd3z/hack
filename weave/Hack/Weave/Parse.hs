-- |Parse a weave file.
--
-- Weave files are in a fairly simple format, line based, with the
-- first character indicating possibly the purpose of the line.  The
-- format is as follows:
--
-- - Lines that start with a Ctrl-A (codepoint 1) are command lines,
--   the second character indicates the purpose of the line:
--   - 't' - The header, the rest of the line is a JSON-encoded
--     "Header".
--   - 'I' - Followed by space and a decimal number.  Indicates that
--     the given delta has insert lines.
--   - 'D' - Followed by a space and a number.  Indicates that the
--     given delta has removed lines below.
--   - 'E' - Followed by a space and a number.  Indicates that the
--     given delta is no longer relevant.
-- Any other lines are just lines of the text that may or may not be
-- included in a given delta, based on the indicator lines preceding
-- it.

{-# LANGUAGE OverloadedStrings #-}

module Hack.Weave.Parse where

import Codec.Compression.GZip (decompress)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as CL
import Hack.Weave.Header (Header(..))
import Text.Read (readMaybe)

-- |The stream is decoded (lazily) into a sequence of the following.
data WeaveElement
   = WeaveHeader Header
   | WeaveInsert Int
   | WeaveDelete Int
   | WeaveEnd Int
   | WeavePlain L.ByteString
   | WeaveError L.ByteString
   deriving Show

readWeaveFile :: FilePath -> IO [WeaveElement]
readWeaveFile path = do
   comp <- L.readFile path
   return $ weaveDecode $ decompress comp

-- |Decode a weave file into its elements.
weaveDecode :: L.ByteString -> [WeaveElement]
weaveDecode = map decodeOne . CL.lines

decodeOne :: L.ByteString -> WeaveElement
decodeOne line
   | CL.isPrefixOf "\^At" line = getHeader line
   | CL.isPrefixOf "\^AI " line = getNum WeaveInsert line
   | CL.isPrefixOf "\^AD " line = getNum WeaveDelete line
   | CL.isPrefixOf "\^AE " line = getNum WeaveEnd line
   | CL.isPrefixOf "\^A" line = WeaveError line
   | otherwise = WeavePlain line

getHeader :: L.ByteString -> WeaveElement
getHeader line = maybe (WeaveError line) WeaveHeader $ decode $ CL.drop 2 line

getNum :: (Int -> WeaveElement) -> L.ByteString -> WeaveElement
getNum enc line =
   maybe (WeaveError line) enc $ readMaybe $ CL.unpack $ CL.drop 3 line

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

import Data.Aeson (decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import Hack.Weave.Header (Header(..))
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import Text.Read (readMaybe)

-- |The stream is decoded (lazily) into a sequence of the following.
data WeaveElement
   = WeaveHeader Header
   | WeaveInsert Int
   | WeaveDelete Int
   | WeaveEnd Int
   | WeavePlain B.ByteString
   | WeaveError B.ByteString
   deriving Show

readWeaveFile :: FilePath -> IO [WeaveElement]
readWeaveFile path = do
   Streams.withFileAsInput path $ \inp -> do
      uninp <- Streams.gunzip inp
      weaveDecode uninp >>= Streams.toList

weaveDecode :: InputStream B.ByteString -> IO (InputStream WeaveElement)
weaveDecode inp = do
   asLines <- Streams.lines inp
   Streams.makeInputStream $ gen asLines
   where
      gen asLines = do
         line <- Streams.read asLines
         return $ fmap decodeOne line

decodeOne :: B.ByteString -> WeaveElement
decodeOne line
   | C8.isPrefixOf "\^At" line = getHeader line
   | C8.isPrefixOf "\^AI " line = getNum WeaveInsert line
   | C8.isPrefixOf "\^AD " line = getNum WeaveDelete line
   | C8.isPrefixOf "\^AE " line = getNum WeaveEnd line
   | C8.isPrefixOf "\^A" line = WeaveError line
   | otherwise = WeavePlain line

getHeader :: B.ByteString -> WeaveElement
getHeader line = maybe (WeaveError line) WeaveHeader $ decode $
   LB.fromStrict $ C8.drop 2 line

getNum :: (Int -> WeaveElement) -> B.ByteString -> WeaveElement
getNum enc line =
   maybe (WeaveError line) enc $ readMaybe $ C8.unpack $ C8.drop 3 line

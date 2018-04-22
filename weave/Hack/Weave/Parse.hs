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

module Hack.Weave.Parse (
   WeaveElement(..),
   readWeaveFile,
   readZWeaveFile,
   weaveParse
) where

import Data.Aeson (decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.IORef
import Hack.Weave.Header (Header(..))
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import Text.Read (readMaybe)

-- |The stream is decoded into a Stream of the following.  The
-- WeavePlain lines will eventually get line number and 'keep'
-- information associated with them, although this is added later.
data WeaveElement
   = WeaveHeader Header
   | WeaveInsert Int
   | WeaveDelete Int
   | WeaveEnd Int
   | WeavePlain B.ByteString (Maybe Int)
   | WeaveError B.ByteString
   deriving Show

readZWeaveFile :: FilePath -> Int -> IO [WeaveElement]
readZWeaveFile path delta = do
   Streams.withFileAsInput path $ \inp -> do
      uninp <- Streams.gunzip inp
      weaveDecode uninp >>= weaveParse delta >>= Streams.toList

readWeaveFile :: FilePath -> Int -> IO [WeaveElement]
readWeaveFile path delta = do
   Streams.withFileAsInput path $ \inp -> do
      weaveDecode inp >>= weaveParse delta >>= Streams.toList

-- |By tracking the insert/delete/end state, augment any "plain" lines
-- adding a line number, and a flag indicating if it should be kept by
-- for the specified delta.  Nothing indicates not to keep, and Just n
-- indicates it should be kept, with the given line number.
--
-- TODO: This is written rather imperatively, mostly because of how
-- streams seems to work.
weaveParse :: Int -> InputStream WeaveElement -> IO (InputStream WeaveElement)
weaveParse delta source = do
   state <- newIORef Map.empty
   lineno <- newIORef 0
   keeping <- newIORef False
   let
      update elt = do
         st <- readIORef state
         writeIORef keeping $ keepState st
         return elt
      gen = do
         elt <- Streams.read source
         case elt of
            Just (WeaveInsert dl) -> do
               let nstate = if delta >= dl then Keep else Skip
               modifyIORef' state (Map.insert dl nstate)
               update elt
            Just (WeaveDelete dl) -> do
               let nstate = if delta >= dl then Skip else Next
               modifyIORef' state (Map.insert dl nstate)
               update elt
            Just (WeaveEnd dl) -> do
               modifyIORef' state (Map.delete dl)
               update elt
            Just (WeavePlain text _) -> do
               kp <- readIORef keeping
               if kp then do
                  modifyIORef' lineno (1+)
                  ln <- readIORef lineno
                  return $ Just $ WeavePlain text (Just ln)
               else do
                  return $ Just $ WeavePlain text Nothing
            other -> return other
   Streams.makeInputStream gen

data State = Keep | Skip | Next

-- Walk through states, which must be in order by largest delta,
-- taking the first of "Keep" or "Skip", as true/false, ignoring any
-- that are next.  Returns false if there are no entries.
keepState :: Map Int State -> Bool
keepState = Map.foldl op False
   where
      op _ Keep = True
      op _ Skip = False
      op b Next = b

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
   | otherwise = WeavePlain line Nothing

getHeader :: B.ByteString -> WeaveElement
getHeader line = maybe (WeaveError line) WeaveHeader $ decode $
   LB.fromStrict $ C8.drop 2 line

getNum :: (Int -> WeaveElement) -> B.ByteString -> WeaveElement
getNum enc line =
   maybe (WeaveError line) enc $ readMaybe $ C8.unpack $ C8.drop 3 line

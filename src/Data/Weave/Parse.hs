-- |Parsing weave files.
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
--
-- The parser is a Pipe that transforms ByteStrings that are lines of
-- the weave file into WeaveElements.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.Weave.Parse (
    readDelta,
    onlyPlain,
) where

import Conduit
import Data.Aeson (decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Text.Read (readMaybe)

import Data.Weave.Types

readDelta :: Monad m => Int -> ConduitT B.ByteString WeaveElement m ()
readDelta num = decodePipe .| getDelta num

-- |Decode elements.  It expects each ByteString to be a single line,
-- and results in the weave elements from the stream.
decodePipe :: Monad m => ConduitT B.ByteString WeaveElement m ()
decodePipe = mapC decodeOne

-- |Only get included plaintext lines.
onlyPlain :: Monad m => ConduitT WeaveElement B.ByteString m ()
onlyPlain = awaitForever $ \elt -> do
    case elt of
        (WeavePlain text (Just _)) -> yield text
        _                          -> return ()

-- |By tracking the insert/delete/end state, augment any "plain"
-- lines, adding a line number, and a flag indicating if it should be
-- kept by the specified delta.  Nothing indicates not to keep, and
-- 'Just n' indicates that it should be kept, with the given line
-- number.
getDelta :: Monad m => Int -> ConduitT WeaveElement WeaveElement m ()
getDelta delta = loop Map.empty 0 False
    where
        loop states lineno keeping = do
            elt <- await
            case elt of
                Just node@(WeaveInsert dl) -> do
                    let nstate = if delta >= dl then Keep else Skip
                    let states' = Map.insert dl nstate states
                    yield node
                    loop states' lineno (keepState states')
                Just node@(WeaveDelete dl) -> do
                    let nstate = if delta >= dl then Skip else Next
                    let states' = Map.insert dl nstate states
                    yield node
                    loop states' lineno (keepState states')
                Just node@(WeaveEnd dl) -> do
                    let states' = Map.delete dl states
                    yield node
                    loop states' lineno (keepState states')
                Just (WeavePlain text _) -> do
                    if keeping then do
                        yield $ WeavePlain text (Just $ lineno + 1)
                        loop states (lineno + 1) keeping
                    else do
                        yield $ WeavePlain text Nothing
                        loop states lineno keeping
                Just other -> do
                    yield other
                    loop states lineno keeping
                Nothing -> return ()

data State = Keep | Skip | Next

-- Walk through states, which must be in order by largest delta,
-- talking the first of "Keep" or "Skip", as true/false, ignoring any
-- that are Next.  Returns false if there are no entries.
keepState :: Map Int State -> Bool
keepState = Map.foldl op False
    where
        op _ Keep = True
        op _ Skip = False
        op b Next = b

-- |Decode a single input line into a weave element.
decodeOne :: B.ByteString -> WeaveElement
decodeOne line
    | C8.isPrefixOf "\^At" line  = getHeader line
    | C8.isPrefixOf "\^AI " line = getNum WeaveInsert line
    | C8.isPrefixOf "\^AD " line = getNum WeaveDelete line
    | C8.isPrefixOf "\^AE " line = getNum WeaveEnd line
    | C8.isPrefixOf "\^A" line   = WeaveError line
    | otherwise                  = WeavePlain line Nothing

-- Extract the header from a header line
getHeader :: B.ByteString -> WeaveElement
getHeader line = maybe (WeaveError line) WeaveHeader $ decode $
    LB.fromStrict $ C8.drop 2 line

-- Extract one of the numbered control lines
getNum :: (Int -> WeaveElement) -> B.ByteString -> WeaveElement
getNum enc line =
    maybe (WeaveError line) enc $ readMaybe $ C8.unpack $ C8.drop 3 line

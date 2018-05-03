{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Hack.Weave.Header (
   Header(..),
   DeltaInfo(..),

   addDelta,
   highestDelta,
) where

import Data.Aeson.TH
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, UTCTime)

data Header = Header {
   version :: !Int,
   deltas :: [DeltaInfo] }
   deriving (Show)

data DeltaInfo = DeltaInfo {
   name :: !Text,
   -- ^A tag giving the name for this particular delta.  Should be
   -- unique across all deltas.
   number :: !Int,
   -- ^The delta number.  A unique integer that identifies this delta
   -- in the woven data below.  Should be a positive integer, starting
   -- with '1'.
   tags :: Map Text Text,
   -- ^Arbitrary tags the user has asked to be stored with this delta.
   time :: !UTCTime
   -- ^A time stamp when this delta was added.
}
   deriving (Show)

deriveJSON defaultOptions ''Header
deriveJSON defaultOptions ''DeltaInfo

-- |Add a new delta to the given header.
addDelta :: Text -> Map Text Text -> Header -> IO Header
addDelta name tags hdr = do
   now <- getCurrentTime
   let dlt = deltas hdr
   let newDelta = DeltaInfo {
      name = name,
      number = highestDelta hdr + 1,
      tags = tags,
      time = now }
   return $ hdr { deltas = newDelta : dlt }

-- Given some deltas, returns the highest delta number, or zero if
-- there are none.
highestDelta :: Header -> Int
highestDelta Header{ deltas = [] } = 0
highestDelta Header{deltas} = maximum $ map number deltas

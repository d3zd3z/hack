{-# LANGUAGE TemplateHaskell #-}

module Hack.Weave.Header (
   Header(..),
   DeltaInfo(..),
) where

import Data.Aeson.TH
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

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

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config.Types (
   Config(..),
   Snap(..),
   Convention(..),
   Volume(..),
   Sure(..),
   SureVolume(..),
   Clone(..),
   CloneVolume(..)
) where

import Data.Aeson.TH
import Data.Text (Text)

data Config = Config {
   snap :: Snap,
   sure :: Sure,
   clone :: Clone }
   deriving (Show)

data Snap = Snap {
   conventions :: [Convention],
   volumes :: [Volume] }
   deriving (Show)

data Convention = Convention {
   name :: Text,
   last :: Maybe Int,
   hourly :: Maybe Int,
   daily :: Maybe Int,
   weekly :: Maybe Int,
   monthly :: Maybe Int,
   yearly :: Maybe Int }
   deriving (Show)

data Volume = Volume {
   name :: Text,
   convention :: Text,
   zfs :: Text }
   deriving Show

data Sure = Sure {
   volumes :: [SureVolume] }
   deriving (Show)

data SureVolume = SureVolume {
   name :: Text,
   zfs :: Text,
   bind :: Text,
   sure :: Text,
   convention :: Text }
   deriving (Show)

data Clone = Clone {
   volumes :: [CloneVolume] }
   deriving (Show)

data CloneVolume = CloneVolume {
   name :: Text,
   source :: Text,
   dest :: Text,
   skip :: Maybe Bool }
   deriving (Show)

-- Note that these have to be after everything is defined, as the TH
-- will only see declarations that come earlier in the file.
deriveJSON defaultOptions ''Config
deriveJSON defaultOptions ''Snap
deriveJSON defaultOptions ''Convention
deriveJSON defaultOptions ''Volume
deriveJSON defaultOptions ''Sure
deriveJSON defaultOptions ''SureVolume
deriveJSON defaultOptions ''Clone
deriveJSON defaultOptions ''CloneVolume

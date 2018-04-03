{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?))

loadConfig :: FilePath -> IO (Either Y.ParseException Config)
loadConfig = Y.decodeFileEither

data Config = Config {
   cSnap :: Snap,
   cSure :: Sure,
   cClone :: Clone }
   deriving (Show)

instance FromJSON Config where
   parseJSON (Y.Object v) = Config <$>
      v .: "snap" <*>
      v .: "sure" <*>
      v .: "clone"
   parseJSON invalid = typeMismatch "Config" invalid

data Snap = Snap {
   snConventions :: [Convention],
   snVolumes :: [Volume] }
   deriving (Show)

instance FromJSON Snap where
   parseJSON (Y.Object v) = Snap <$>
      v .: "conventions" <*>
      v .: "volumes"
   parseJSON invalid = typeMismatch "Snap" invalid

data Convention = Convention {
   cName :: Text,
   cLast :: Maybe Int,
   cHourly :: Maybe Int,
   cDaily :: Maybe Int,
   cWeekly :: Maybe Int,
   cMonthly :: Maybe Int,
   cYearly :: Maybe Int }
   deriving (Show)

instance FromJSON Convention where
   parseJSON (Y.Object v) = Convention <$>
      v .: "name" <*>
      v .:? "last" <*>
      v .:? "hourly" <*>
      v .:? "daily" <*>
      v .:? "weekly" <*>
      v .:? "monthly" <*>
      v .:? "yearly"
   parseJSON invalid = typeMismatch "Convention" invalid

data Volume = Volume {
   vName :: Text,
   vConvention :: Text,
   vZfs :: Text }
   deriving Show

instance FromJSON Volume where
   parseJSON (Y.Object v) = Volume <$>
      v .: "name" <*>
      v .: "convention" <*>
      v .: "zfs"
   parseJSON invalid = typeMismatch "Volume" invalid

data Sure = Sure {
   sVolumes :: [SureVolume] }
   deriving (Show)

instance FromJSON Sure where
   parseJSON (Y.Object v) = Sure <$>
      v .: "volumes"
   parseJSON invalid = typeMismatch "Sure" invalid

data SureVolume = SureVolume {
   svName :: Text,
   svZfs :: Text,
   svBind :: Text,
   svSure :: Text,
   svConvention :: Text }
   deriving (Show)

instance FromJSON SureVolume where
   parseJSON (Y.Object v) = SureVolume <$>
      v .: "name" <*>
      v .: "zfs" <*>
      v .: "bind" <*>
      v .: "sure" <*>
      v .: "convention"
   parseJSON invalid = typeMismatch "SureVolume" invalid

data Clone = Clone {
   cVolumes :: [CloneVolume] }
   deriving (Show)

instance FromJSON Clone where
   parseJSON (Y.Object v) = Clone <$>
      v .: "volumes"
   parseJSON invalid = typeMismatch "Clone" invalid

data CloneVolume = CloneVolume {
   cvName :: Text,
   cvSource :: Text,
   cvDest :: Text,
   cvSkip :: Maybe Bool }
   deriving (Show)

instance FromJSON CloneVolume where
   parseJSON (Y.Object v) = CloneVolume <$>
      v .: "name" <*>
      v .: "source" <*>
      v .: "dest" <*>
      v .:? "skip"
   parseJSON invalid = typeMismatch "CloneVolume" invalid


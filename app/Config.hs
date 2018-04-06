module Config (
   module Config.Types,
   loadConfig
) where

import Config.Types

import qualified Data.Yaml as Y

loadConfig :: FilePath -> IO (Either Y.ParseException Config)
loadConfig = Y.decodeFileEither

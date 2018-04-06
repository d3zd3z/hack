module Config (
   module Config.Types,
   loadConfig,
   loadConfigError
) where

import Config.Types
import Control.Exception (throwIO)

import qualified Data.Yaml as Y

-- |Load a config file of the given path, returning an Either with the
-- Right as the config value.
loadConfig :: FilePath -> IO (Either Y.ParseException Config)
loadConfig = Y.decodeFileEither

-- |Load as in loadConfig, but throw an exception in the IO monad if
-- there is an error.
loadConfigError :: FilePath -> IO Config
-- loadConfigError = either throwIO return . loadConfig
loadConfigError fname = do
   conf <- loadConfig fname
   either throwIO return conf

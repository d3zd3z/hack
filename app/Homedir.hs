module Homedir where

import Path (parseRelFile, toFilePath, (</>))
import Path.IO (getHomeDir)

-- |Expand a leading single ~/ in a filename to the user's home
-- directory.
expandTilde :: FilePath -> IO FilePath
expandTilde ('~':'/':name) = do
   home <- getHomeDir
   rname <- parseRelFile name
   return $ toFilePath $ home </> rname
expandTilde name = return name

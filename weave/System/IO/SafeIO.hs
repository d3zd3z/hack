-- |Safe IO operation.
--
-- The regular Haskell System.IO file operations don't have a way to
-- open a file, failing if the file already exists.  This module uses
-- the Posix library to open a file handle safely, raising an
-- exception if the file already exists.

module System.IO.SafeIO (
   openExclusiveWrite
) where

import System.IO (Handle)
import System.Posix.IO (openFd, fdToHandle, OpenMode(..), OpenFileFlags(..), defaultFileFlags)

-- |This returns a new open handle to manage the given file.  The file
-- will be opened in WriteMode.  The file will be created if it does
-- not exist, and if it already exists, this will raise a IOError
-- exception.
openExclusiveWrite :: FilePath -> IO Handle
openExclusiveWrite name = do
   fd <- openFd name WriteOnly (Just mode) flags
   fdToHandle fd
   where
      mode = 0o666
      flags = defaultFileFlags { exclusive = True }

-- |Weave file naming conventions

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hack.Weave.Naming (
   Naming(..),
   SimpleNaming(..),
   openTemp,
   withTemp
) where

import Control.Exception (bracket, throwIO, try)
import System.IO (Handle, hClose)
import System.IO.Error (IOError)
import System.IO.SafeIO (openExclusiveWrite)

-- |Naming is a is some type that can be used to generate filenames.
-- It knows whether files are preferred to be compressed, and can
-- generate names.
class Show a => Naming a where
   -- | Returns if this naming convention prefers the weave files to
   -- be compressed.
   isNameCompressed :: a -> Bool

   -- | @n ext comp@ returns a full path to a filename using this
   -- naming convention.  The file will have @ext@ as an extension,
   -- possibly with a compression suffix if @comp@ is true.  This
   -- function only generates the name, and does not perform any
   -- safety checks on prior existence.
   genName :: a -> FilePath -> Bool -> FilePath

   -- |Returns the preferred extension for the main file in this
   -- naming convention.
   preferredExtension :: a -> FilePath

-- |A simple naming convention that has a path prefix (directory, and
-- base of filename)
data SimpleNaming = SimpleNaming {
   snPrefix :: FilePath,
   snExt :: FilePath, -- Preferred extension for main data.
   snCompressed :: Bool }
   deriving Show

instance Naming SimpleNaming where
   isNameCompressed = snCompressed
   genName SimpleNaming{..} ext comp =
      snPrefix ++ "." ++ ext ++ (if comp then ".gz" else "")
   preferredExtension = snExt

-- | 'openTemp' @naming comp@ opens a new file using the given naming
-- convention, with possible compression.  Will return a name that
-- doesn't exist yet, and the file will be opened.  Returns the newly
-- opened name, and an open-for-write handle.
openTemp :: Naming n => n -> Bool -> IO (FilePath, Handle)
openTemp naming comp = loop 1
   where
      loop :: Int -> IO (FilePath, Handle)
      loop n = do
         if n > 200 then throwIO $ userError $ "Unable to create temp file: " ++ show naming
         else do
            let name = genName naming (show n) comp
            efd <- try $ openExclusiveWrite name
            either (\(_ :: IOError) -> loop (n + 1))
               (\fd -> return (name, fd))
               efd

-- | 'withTemp' @naming comp action@ invokes action with a newly
-- created temp file.
withTemp :: Naming n => n -> Bool -> (FilePath -> Handle -> IO a) -> IO a
withTemp naming comp = bracket (openTemp naming comp) (hClose . snd) . uncurry

-- A test naming
{-
tname = SimpleNaming "./haha" "dat" True
t1 = do
   (n1, h1) <- openTemp tname False
   putStrLn n1
   hClose h1
   (n2, h2) <- openTemp tname False
   putStrLn n2
   hClose h2
-}

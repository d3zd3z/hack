-- |Naming convention types.

{-# LANGUAGE RecordWildCards #-}

module Data.Weave.Naming.Internal (
    Naming(..),
    SimpleNaming(..),
) where

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

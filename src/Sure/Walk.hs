-- |Filesystem walking

{-# LANGUAGE OverloadedStrings #-}

module Sure.Walk (
   SureTree(..),
   SureFile(..),
   walk
) where

import Sure.Atts (AttMap, getAtts, isDir)

import Control.Exception (bracket, throwIO, tryJust)
import Control.Monad (guard, forM, unless)
import qualified Data.ByteString.Char8 as B
import Data.List (partition, sortOn)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Vector as V
import Data.Vector (Vector)
import System.IO.Error (isUserError)
import qualified System.Posix.ByteString as P

-- |The top of a suretree.
data SureTree = SureTree {
   stName :: !B.ByteString,
   stAtts :: !AttMap,
   stChildren :: !(Vector SureTree),
   stFiles :: !(Vector SureFile) }
   deriving Show

data SureFile = SureFile {
   sfName :: !B.ByteString,
   sfAtts :: !AttMap }
   deriving Show

-- |Walk a filesystem, loading a tree into memory of all of the easily
-- gathered information.  The path should be a name that will resolve
-- from the current directory (relative from there, or absolute).
walk :: P.RawFilePath -> IO SureTree
walk path = do
   stat <- P.getSymbolicLinkStatus path
   atts <- getAtts path stat
   unless (isDir atts) $ do
      throwIO $ userError $ "Given path is not a directory: " ++ show path
   walkDir path atts

-- |Walk a particular directory, having retrieve the attributes for
-- it.
walkDir :: P.RawFilePath -> AttMap -> IO SureTree
walkDir path atts = do
   -- putStrLn $ "Walking: " ++ show path
   (dirs1, nondirs1) <- (partition (isDir . snd)) <$> getDir path
   let dirs = sortOn fst dirs1
   let nondirs = sortOn fst nondirs1
   children <- subWalk path dirs
   return $ SureTree {
      stName = path,
      stAtts = atts,
      stChildren = V.fromList children,
      stFiles = V.fromList $ map (uncurry SureFile) nondirs }

subWalk :: P.RawFilePath -> [(P.RawFilePath, AttMap)] -> IO [SureTree]
subWalk dirPath = mapM (\(name, atts) -> walkDir (dirPath <> "/" <> name) atts)

-- |Get all of the names in a given directory, stat all of them (in
-- dir order), and sort and return the ones that were possible to
-- stat.
getDir :: P.RawFilePath -> IO [(P.RawFilePath, AttMap)]
getDir dir = do
   names <- maybe [] id <$> safely (getDirEntries dir)
   stats <- mapM (statFile dir) names
   forM (catMaybes $ map liftMaybe2 $ zip names stats) $ \(name, stat) -> do
      atts <- getAtts name stat
      return $ (name, atts)

-- |Read all of the names of entities in a given directory.  Removes
-- "." and ".." from the results.
getDirEntries :: P.RawFilePath -> IO [P.RawFilePath]
getDirEntries dName = do
   bracket (P.openDirStream dName) P.closeDirStream $ \ds -> loop ds []
   where
      loop ds xs = do
         name <- P.readDirStream ds
         case name of
            _ 
               | B.null name -> return xs
               | name == "." || name == ".." -> loop ds xs
               | otherwise -> loop ds (name:xs)

-- |Try statting a file in a given directory
statFile :: P.RawFilePath -> P.RawFilePath -> IO (Maybe P.FileStatus)
statFile dir name = do
   let path = dir <> "/" <> name
   safely $ P.getSymbolicLinkStatus path

-- |Perform the operation, returning Just item if it succeeds, and
-- Nothing if it raises a non-user IOError.  Any other type of
-- exception will be just raised.
safely :: IO a -> IO (Maybe a)
safely op = do
   r <- tryJust (guard . not . isUserError) op
   return $ either (const Nothing) Just r

liftMaybe2 :: (a, Maybe b) -> Maybe (a, b)
liftMaybe2 (_, Nothing) = Nothing
liftMaybe2 (a, Just b) = Just (a, b)

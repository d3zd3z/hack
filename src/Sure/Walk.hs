-- |Filesystem walking

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Sure.Walk (
   SureNode(..),
   walk,

   safely
) where

import Sure.Atts (AttMap, getAtts, isDir)

import Conduit
import Control.Exception (bracket, throwIO, tryJust)
import Control.Monad (guard, forM, unless)
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Char8 as B
import Data.List (partition, sortOn)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import System.IO.Error (isUserError)
import qualified System.Posix.ByteString as P

import Text.Human
import Text.Progress
import Sure.Types (SureNode(..))

-- The progress meter.
data Meter = Meter {
   mPm :: !PMeter,
   mDirs :: !Integer,
   mFiles :: !Integer,
   mBytes :: !Integer }

type MeterIO = StateT Meter (ConduitT () SureNode IO)

-- |A stream that walks a filesystem, yielding all of the in an
-- in-order depth-first traversal of the tree.
walk :: P.RawFilePath -> ConduitT () SureNode IO ()
walk path = do
    withPMeter $ \pm -> do
        let meter = Meter {
            mPm = pm,
            mDirs = 0,
            mFiles = 0,
            mBytes = 0 }
        evalStateT action meter
    where
        -- action :: MonadIO m => MeterIO m
        action = do
            stat <- liftIO $ P.getSymbolicLinkStatus path
            atts <- liftIO $ getAtts path stat
            unless (isDir atts) $ do
                liftIO $ throwIO $ userError $ "Given path is not a directory: " ++ show path
            -- TODO: Fix the root name.
            walkDir path "__root__" atts

walkDir :: P.RawFilePath -> P.RawFilePath -> AttMap -> MeterIO ()
walkDir path name atts = do
    (dirs1, nondirs1) <- (partition (isDir . snd)) <$> getDir path
    let dirs = sortOn fst dirs1
    let nondirs = sortOn fst nondirs1
    lift $ yield $ SureEnter name atts
    subWalk path dirs
    lift $ yield SureSep
    lift $ yieldMany $ map (uncurry SureNode) nondirs
    lift $ yield SureLeave

subWalk :: P.RawFilePath -> [(P.RawFilePath, AttMap)] -> MeterIO ()
subWalk dirPath = mapM_ (\(name, atts) -> walkDir (dirPath <> "/" <> name) name atts)

getDir :: P.RawFilePath -> MeterIO [(P.RawFilePath, AttMap)]
getDir dir = do
    names <- liftIO (maybe [] id <$> safely (getDirEntries dir))
    stats <- liftIO $ mapM (statFile dir) names
    update $ catMaybes stats
    liftIO $ forM (catMaybes $ map liftMaybe2 $ zip names stats) $ \(name, stat) -> do
        atts <- getAtts name stat
        return $ (name, atts)

-- |Update the statistics based on the stats given.
update :: [P.FileStatus] -> MeterIO ()
update stats = do
   let (dirs, files) = partition P.isDirectory stats
   let bytes = sum $ map (fromIntegral . P.fileSize) files
   pm <- get
   let newDirs = mDirs pm + (fromIntegral $ length dirs)
   let newFiles = mFiles pm + (fromIntegral $ length files)
   let newBytes = bytes + mBytes pm
   liftIO $ pmPut (mPm pm) $ "scan: " ++ show newDirs ++
      " dirs, " ++ show newFiles ++
      " files, " ++ humanizeBytes newBytes ++ " bytes"
   put $! pm {
      mDirs = newDirs,
      mFiles = newFiles,
      mBytes = newBytes }

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

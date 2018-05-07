-- |File system integrity.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Sure (
    scan,

    basicNaming,
    simpleWalk,
    oldWalk,
    estimateHashes,
    updateHashes,
    simpleSignoff,
) where

import Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

import Data.Weave
import Sure.Decode (sureNodeParser)
import Sure.Encode (sureEncoder)
import qualified Sure.Hashes as SH
import Sure.Compare
import Sure.Hashes (HashProgress(..))
import Sure.Types (addDirs)
import Sure.Signoff
import Sure.Walk (walk)
import Text.Progress (withPMeter)

basicNaming :: SimpleNaming
basicNaming = SimpleNaming "./2sure" "dat" True

-- |Perform a 'scan' of the directory, using the naming convention
-- specified.  A scan will always hash every file, regardless of
-- whether there are already scans in the surefile.
scan :: Naming n => n -> B.ByteString -> IO ()
scan naming dirName = do
    runResourceT $ do
        t1 <- simpleWalk naming dirName
        est <- estimateHashes t1
        t2 <- updateHashes naming est t1 dirName
        liftIO $ putStrLn $ "Scan in: " ++ show t2

-- Walk a filesystem at 'dirName', dumping the scan data into a new temp
-- file, and return the name of the temp file.
simpleWalk :: (Naming n, MonadResource m) => n -> B.ByteString -> m FilePath
simpleWalk naming dirName = do
    runConduit $ transPipe liftIO (walk dirName) .|
        toTempFile naming (\tname -> sureEncoder >> return tname)

-- Walk a filesystem at 'dirName', while also looking at 'oldFile' to
-- reuse any hashes we might be able to get from there.
oldWalk :: (Naming n, MonadResource m) => n -> B.ByteString -> FilePath -> m FilePath
oldWalk naming dirName oldName = do
    runConduit $ do
        let srcNew = transPipe liftIO (walk dirName)
        let srcOld = fromTempFile oldName .| sureNodeParser
        combineTrees srcOld srcNew .| SH.combineHashes .|
            toTempFile naming (\tname -> sureEncoder >> return tname)

-- Read a temp file containing a walk, and build an estimate of the
-- number and size of hashes that need computation.
estimateHashes :: MonadResource m => FilePath -> m HashProgress
estimateHashes path = do
    runConduit $ fromTempFile path .| sureNodeParser .| SH.estimateHashes

-- |Update the hashes, for any nodes that don't have hashes.
updateHashes
    :: (Naming n, MonadResource m)
    => n
    -> HashProgress
    -> FilePath
    -> B.ByteString
    -> m FilePath
updateHashes naming hp path rootDir = do
    runConduit $ withPMeter $ \meter -> do
        fromTempFile path .| sureNodeParser .| addDirs rootDir .|
            SH.computeHashes meter hp .|
            toTempFile naming (\tname -> sureEncoder >> return tname)

simpleSignoff :: FilePath -> FilePath -> IO ()
simpleSignoff oldPath newPath = do
    runConduitRes $ do
        let srcOld = fromTempFile oldPath .| sureNodeParser
        let srcNew = fromTempFile newPath .| sureNodeParser
        combineTrees srcOld srcNew .| signoff .|
            mapC (C8.pack . prettyNode) .| unlinesAsciiC .| stdoutC

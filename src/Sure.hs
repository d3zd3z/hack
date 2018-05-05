-- |File system integrity.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Sure (
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

-- Walk a filesystem at 'dirName', dumping the scan data into a new temp
-- file, and return the name of the temp file.
simpleWalk :: Naming n => n -> B.ByteString -> IO FilePath
simpleWalk naming dirName = do
    runConduitRes $ transPipe liftIO (walk dirName) .|
        toTempFile naming (\tname -> sureEncoder >> return tname)

-- Walk a filesystem at 'dirName', while also looking at 'oldFile' to
-- reuse any hashes we might be able to get from there.
oldWalk :: Naming n => n -> B.ByteString -> FilePath -> IO FilePath
oldWalk naming dirName oldName = do
    runConduitRes $ do
        let srcNew = transPipe liftIO (walk dirName)
        let srcOld = fromTempFile oldName .| sureNodeParser
        combineTrees srcOld srcNew .| SH.combineHashes .|
            toTempFile naming (\tname -> sureEncoder >> return tname)

-- Read a temp file containing a walk, and build an estimate of the
-- number and size of hashes that need computation.
estimateHashes :: FilePath -> IO HashProgress
estimateHashes path = do
    runConduitRes $ fromTempFile path .| sureNodeParser .| SH.estimateHashes

-- |Update the hashes, for any nodes that don't have hashes.
updateHashes :: Naming n => n -> HashProgress -> FilePath -> B.ByteString -> IO FilePath
updateHashes naming hp path rootDir = do
    runConduitRes $ withPMeter $ \meter -> do
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

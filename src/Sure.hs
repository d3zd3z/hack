-- |File system integrity.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Sure (
    basicNaming,
    simpleWalk,
    oldWalk,
    estimateHashes,
    updateHashes,
) where

import Conduit
import qualified Data.ByteString as B

import Data.Weave
import Sure.Decode (sureNodeParser)
import Sure.Encode (sureEncoder)
import qualified Sure.Hashes as SH
import Sure.Compare
import Sure.Hashes (HashProgress(..))
import Sure.Types (addDirs)
import Sure.Walk (walk)
import Text.Progress (withPMeter)

basicNaming :: SimpleNaming
basicNaming = SimpleNaming "./2sure" "dat" True

-- TODO: Convert Naming operations to resources.

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
{-
oldWalk naming dirName oldName = do
    toTempFile naming $ \sink tname -> do
        fromTempFile oldName $ \srcOld -> do
            let srcNew = walk dirName
            let out = sureEncoder >-> sink
            runEffect $ SH.combineHashes (srcOld >-> sureNodeParser) srcNew >-> out
            return tname
-}

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
{-
updateHashes naming hp path rootDir = do
    fromTempFile path $ \src -> do
        toTempFile naming $ \sink tname -> do
            withPMeter $ \meter -> do
                let inp = src >-> sureNodeParser >-> addDirs rootDir
                let outp = sureEncoder >-> sink
                runEffect $ inp >-> SH.computeHashes meter hp >-> outp
                return tname
-}

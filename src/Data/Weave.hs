-- |Weaves

{-# LANGUAGE OverloadedStrings #-}

module Data.Weave (
    Naming(..),
    SimpleNaming(..),
    firstDelta,
    toTempFile,
    fromTempFile,
    readDelta,

    getWeaveInfo,
    addDeltaFile,
) where

import Conduit
import Control.Exception (tryJust)
import Control.Monad (guard)
import Control.Monad.Trans.Resource (liftResourceT, register)
import qualified Data.ByteString as B
import Data.Conduit.Zlib (gzip)
import qualified Data.Map.Strict as Map
import System.Directory (removeFile, renameFile)
import System.IO (hClose)
import System.IO.Error (isDoesNotExistError, isUserError)

import Data.Weave.Header
import Data.Weave.Naming
import Data.Weave.Parse
import Data.Weave.Write

-- |Open a possibly existing weave file, reading in the header.
getWeaveInfo :: Naming n => n -> IO Header
getWeaveInfo n = do
    r <- tryJust (guard . isDoesNotExistError) $ runConduitRes $ openPrimary n .| readHeader
    return $ either (const blankHeader) id r

-- |Add the contents of the given file to the specified naming.
addDeltaFile :: (Naming n, MonadResource m, MonadThrow m, PrimMonad m) => n -> FilePath -> m ()
addDeltaFile n srcname = do
    hdr <- liftIO $ getWeaveInfo n
    let isComp = isNameCompressed n
    if isEmptyHeader hdr then do
        hdr2 <- addDelta "description-here" Map.empty hdr
        -- Write a new delta to a temp file.  Don't delete this temp
        -- file, since we will try to rename it to the original file.
        tname <- runConduit $ do
            bracketP (openTemp n isComp) (hClose . snd) $ \(tname, h) -> do
                let dest1 = sinkHandle h
                let dest = unlinesAsciiC .|
                        if isComp then gzip .| dest1 else dest1
                let src = fromTempFile srcname
                src .| firstDelta hdr2 .| weaveEncode .| dest
                return tname
        -- Rename primary to backup.
        let priName = genName n (preferredExtension n) isComp
        let bakName = genName n "bak" isComp
        _ <- liftIO $ tryJust (guard . not. isUserError) $ renameFile priName bakName
        liftIO $ renameFile tname priName
    else do
        liftIO $ putStrLn $ "TODO: Update delta"

-- TODO: Change to resources to clean this up.

-- |Write lines of data to a temporary file.  The file will not be
-- compressed.  It is up to 'action' to insert its argument into a
-- pipeline, and run the pipeline.  The name of the temp file will be
-- passed to action.  It is generally a good idea to return the name
-- as part of the result of the action.
-- This is a combinator so we can pass the name of the file to the
-- action.
-- This will use the resource to register the deletion of the
-- temporary file when the resource is finished.  The file will be
-- closed as soon as the pipeline is finished, but will stay around
-- until the resource is completed.
toTempFile
    :: (Naming n, MonadResource m)
    => n
    -> (FilePath -> ConduitT a B.ByteString m r)
    -> ConduitT a Void m r
toTempFile naming action = do
    bracketP (openTemp naming False) (hClose . snd) $ \(tname, h) -> do
        _ <- liftResourceT $ register $ removeFile tname
        let pipe = unlinesAsciiC .| sinkHandle h
        (res, _) <- fuseBoth (action tname) pipe
        return $ res

-- |Read lines from a temp file (passed in by name).
fromTempFile
    :: MonadResource m
    => FilePath
    -> ConduitT () B.ByteString m ()
fromTempFile path = sourceFile path .| linesUnboundedAsciiC

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
) where

import Conduit
import Control.Exception (tryJust)
import Control.Monad (guard)
import Control.Monad.Trans.Resource (liftResourceT, register)
import qualified Data.ByteString as B
import System.Directory (removeFile)
import System.IO (hClose)
import System.IO.Error (isDoesNotExistError)

import Data.Weave.Header
import Data.Weave.Naming
import Data.Weave.Parse
import Data.Weave.Write

-- |Open a possibly existing weave file, reading in the header.
getWeaveInfo :: Naming n => n -> IO Header
getWeaveInfo n = do
    r <- tryJust (guard . isDoesNotExistError) $ runConduitRes $ openPrimary n .| readHeader
    return $ either (const blankHeader) id r

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

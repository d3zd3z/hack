-- |Weaves

{-# LANGUAGE OverloadedStrings #-}

module Data.Weave (
    Naming(..),
    SimpleNaming(..),
    outSink,
    firstDelta,
    toTempFile,
    fromTempFile
) where

import Conduit
import qualified Data.ByteString as B
import System.IO (hClose)

import Data.Weave.Naming
import Data.Weave.Parse
import Data.Weave.Write

-- TODO: Change to resources to clean this up.

-- |Write lines of data to a temporary file.  The file will not be
-- compressed.  It is up to 'action' to insert its argument into a
-- pipeline, and run the pipeline.  The name of the temp file will be
-- passed to action.  It is generally a good idea to return the name
-- as part of the result of the action.
-- This is a combinator so we can pass the name of the file to the
-- action.
toTempFile
    :: (Naming n, MonadResource m)
    => n
    -> (FilePath -> ConduitT a B.ByteString m r)
    -> ConduitT a Void m r
toTempFile naming action = do
    bracketP (liftIO $ openTemp naming False) (liftIO . hClose . snd) $ \(tname, h) -> do
        let pipe = unlinesAsciiC .| sinkHandle h
        (res, _) <- fuseBoth (action tname) pipe
        return $ res

-- |Read lines from a temp file (passed in by name).
fromTempFile
    :: MonadResource m
    => FilePath
    -> ConduitT () B.ByteString m ()
fromTempFile path = sourceFile path .| linesUnboundedAsciiC

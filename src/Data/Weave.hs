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

import Control.Lens (view)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Pipes
import Pipes.Group (folds)
import qualified Pipes.ByteString as PB
import System.IO (hClose, openFile, IOMode(..))

import Data.Weave.Naming
import Data.Weave.Parse
import Data.Weave.Write

-- |Write lines of data to a temporary file.  The file will not be
-- compressed.  It is up to 'action' to insert its argument into a
-- pipeline, and run the pipeline.  The name of the temp file will be
-- passed to action.  It is generally a good idea to return the name
-- as part of the result of the action.
toTempFile
    :: (Naming n, MonadIO m, MonadMask m)
    => n
    -> (Consumer B.ByteString m () -> FilePath -> m a)
    -> m a
toTempFile naming action = do
    withTemp naming False $ \tname h -> do
        let pipe = pUnlines >-> PB.toHandle h
        action pipe tname

-- |Read lines from a temp file (passed in by name).  It is up to
-- 'action' to insert the argument into a pipeline, and run the
-- pipeline.
fromTempFile
    :: (MonadIO m, MonadMask m)
    => FilePath
    -> (Producer B.ByteString m () -> m a)
    -> m a
fromTempFile path action = do
    bracket (liftIO $ openFile path ReadMode) (liftIO . hClose) $ \h -> do
        let src = folds mappend B.empty id $ view PB.lines $ PB.fromHandle h
        action src

-- A simple unlines function.  Just interleaves newlines between each
-- line.
pUnlines :: Monad m => Pipe B.ByteString B.ByteString m ()
pUnlines = for cat $ \line -> do
    yield line
    yield "\n"

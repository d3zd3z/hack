-- |File system integrity.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Sure (
    basicNaming,
    simpleWalk,
    estimateHashes,

    showOut
) where

import Control.Lens (view)
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Pipes
import Pipes.Group (folds)
import qualified Pipes.ByteString as PB
import System.IO (Handle, hPutStrLn, withFile, IOMode(..))

import Data.Weave.Naming
import Sure.Decode (sureNodeParser)
import Sure.Encode (sureEncoder)
import qualified Sure.Hashes as SH
import Sure.Hashes (HashProgress(..))
import Sure.Walk (walk)

basicNaming :: SimpleNaming
basicNaming = SimpleNaming "./2sure" "dat" True

-- Walk a filesystem at 'dirName', dumping the scan data into a new temp
-- file, and return the name of the temp file.
simpleWalk :: Naming n => n -> B.ByteString -> IO FilePath
simpleWalk naming dirName = do
    withTemp naming False $ \tname h -> do
        runEffect $ walk dirName >-> sureEncoder >-> pUnlines >-> PB.toHandle h
        return $ tname

-- Read a temp file containing a walk, and build an estimate of the
-- number and size of hashes that need computation.
estimateHashes :: FilePath -> IO HashProgress
estimateHashes path = do
    withFile path ReadMode $ \h -> do
        let lns = folds mappend B.empty id $ view PB.lines $ PB.fromHandle h
        SH.estimateHashes (lns >-> sureNodeParser)

{-
-- Read the scan from the given temp file, feeding it to the specified
-- consumer, returning the result.
inputScan :: MonadIO m => FilePath -> (Consumer SureNode m a) -> m a
inputScan name = do
    withFile name ReadMode $ \h -> do
        runEffect $ PB.fromHandle
-}

-- A simple unlines function
pUnlines :: Monad m => Pipe B.ByteString B.ByteString m ()
pUnlines = for cat $ \line -> do
    yield line
    yield "\n"

-- A dev function that 'shows' things to a temp file.
-- TODO: Enhance "Sure.Encode" to support this.
showOut :: (MonadIO m, Show s) => Handle -> Consumer' s m ()
showOut h =
    for cat $ \piece -> do
        liftIO $ hPutStrLn h $ show piece

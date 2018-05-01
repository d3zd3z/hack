-- |File system integrity.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Sure (
    basicNaming,
    simpleWalk,

    showOut
) where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Pipes
import qualified Pipes.ByteString as PB
import System.IO (Handle, hPutStrLn)

import Data.Weave.Naming
import Sure.Encode (sureEncoder)
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

-- |File system integrity.

{-# LANGUAGE RankNTypes #-}

module Sure (
    basicNaming,
    simpleWalk
) where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Pipes
import System.IO (Handle, hPutStrLn)

import Data.Weave.Naming
import Sure.Walk (walk)

basicNaming :: SimpleNaming
basicNaming = SimpleNaming "./2sure" "dat" True

-- Walk a filesystem at 'dirName', dumping the scan data into a new temp
-- file, and return the name of the temp file.
simpleWalk :: Naming n => n -> B.ByteString -> IO FilePath
simpleWalk naming dirName = do
    withTemp naming False $ \tname h -> do
        runEffect $ walk dirName >-> showOut h
        return $ tname

-- A dev function that 'shows' things to a temp file.
-- TODO: Enhance "Sure.Encode" to support this.
showOut :: (MonadIO m, Show s) => Handle -> Consumer' s m ()
showOut h =
    for cat $ \piece -> do
        liftIO $ hPutStrLn h $ show piece

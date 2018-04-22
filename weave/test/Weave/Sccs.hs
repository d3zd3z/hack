{-# LANGUAGE OverloadedStrings #-}

module Weave.Sccs (
   haveSccs
) where

import Control.Exception (Handler(..), IOException, catches)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import System.Process.Typed (ExitCodeException, proc, readProcess_)
import Text.Regex.TDFA ((=~))

data Env = Env {
   tempDir :: FilePath }

type EnvIO = ReaderT Env IO

-- In order to compare what we generate with what Sccs does, this test
-- invokes the external 'sccs' command

-- |Determine if a reasonable version of sccs is present.
haveSccs :: IO Bool
haveSccs =
   catches run [Handler handleIO, Handler handleExit]
   where
      run :: IO Bool
      run = do
         (_stdout, stderr) <- readProcess_ $ proc "sccs" ["--version"]
         -- putStrLn $ "stdout: " ++ show stdout ++ "\n stderr: " ++ show stderr
         return $ stderr =~ goodSccs

      handleIO :: IOException -> IO Bool
      handleIO e = do
         putStrLn $ "Unable to run 'sccs': " ++ show e
         return False

      handleExit :: ExitCodeException -> IO Bool
      handleExit e = do
         putStrLn $ "Error running 'sccs': " ++ show e
         return False

-- |A Regex that matches the sccs version we find acceptable.
goodSccs :: ByteString
goodSccs = "sccs from GNU CSSC.*"

-- |Output all of the given items as individual lines to the given
-- named file.
saveLines :: Show a => FilePath -> [a] -> IO ()
saveLines fname = writeFile fname . unlines . map show

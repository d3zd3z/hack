{-# LANGUAGE OverloadedStrings #-}

module Weave.Sccs (
   haveSccs,
   runManyDeltas
) where

import Hack.Weave.Parse
import Hack.Weave.Types
import Weave.Gen

import Control.Exception (Handler(..), IOException, catches)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Foldable (for_)
import Data.Maybe (mapMaybe)
import System.Directory (removeFile)
import System.IO (openFile, IOMode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (ExitCodeException, proc, readProcess_, runProcess_,
   setWorkingDir)
import qualified System.Process.Typed as P
import Test.Tasty.HUnit ((@?=))
import Text.Regex.TDFA ((=~))

data Env = Env {
   tempDir :: FilePath }
   deriving Show

type EnvIO = ReaderT Env IO

runManyDeltas :: IO ()
runManyDeltas = do
   withSystemTempDirectory "weave" $ \tdir -> do
      let env = (Env { tempDir = tdir })
      putStrLn $ show env
      runReaderT manyDeltas env

manyDeltas :: EnvIO ()
manyDeltas = do
   let edits = take 50 $ simEdits 1000
   for_ (zip [1..] edits) $ \(delta, vers) -> do
      (if delta == 1 then initVersion else addDelta) vers
      check vers delta

check :: (Show a, Read a, Eq a) => [a] -> Int -> EnvIO ()
check expected delta = do
   name <- (++ "/s.tfile") <$> asks tempDir
   aitems <- lift $ getDelta name delta
   let items = map read aitems
   lift $ expected @?= items

-- Read a weave file of a given delta, and return all of the plain
-- lines.
getDelta :: FilePath -> Int -> IO [String]
getDelta name delta = do
   items <- readWeaveFile name delta
   return $ mapMaybe onlyPlain items
   where
      onlyPlain (WeavePlain text (Just _)) = Just $ C8.unpack text
      onlyPlain _                          = Nothing

-- Write the given sequence to a new file, and create the first sccs
-- version.
initVersion :: Show a => [a] -> EnvIO ()
initVersion items = do
   pn <- plainName
   tdir <- asks tempDir
   lift $ saveLines pn items
   n1 <- lift devNull
   lift $ runProcess_ $
      setWorkingDir tdir $
      P.setStderr n1 $
      proc "sccs" ["admin", "-itfile", "-n", "s.tfile"]
   lift $ removeFile pn

addDelta :: Show a => [a] -> EnvIO ()
addDelta items = do
   pn <- plainName
   tdir <- asks tempDir
   n1a <- lift devNull
   lift $ runProcess_ $
      setWorkingDir tdir $
      P.setStdout n1a $
      proc "sccs" ["get", "-e", "s.tfile"]
   lift $ saveLines pn items
   n1b <- lift devNull
   n2b <- lift devNull
   lift $ runProcess_ $
      setWorkingDir tdir $
      P.setStdout n1b $
      P.setStderr n2b $
      proc "sccs" ["delta", "-yMessage", "s.tfile"]

plainName :: EnvIO FilePath
plainName = do
   t <- asks tempDir
   return $ t ++ "/tfile"

-- Create a process output that writes to /dev/null, and closes it
-- when the process is finished.
devNull :: IO (P.StreamSpec a ())
devNull = do
   n <- openFile "/dev/null" WriteMode
   return $ P.useHandleOpen n

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

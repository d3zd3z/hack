-- |Delta generation for weave files

{-# LANGUAGE OverloadedStrings #-}

module Hack.Weave.Delta (
   t1
) where

import Hack.Weave.Header
import Hack.Weave.Naming
import Hack.Weave.Parse
import Hack.Weave.Types
import Hack.Weave.Write

import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Data.Algorithm.Patience (diff, Item(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime)
import System.Directory (renameFile)
import System.IO.Streams (InputStream, Generator, yield)
import qualified System.IO.Streams as Streams

writeNewWeave :: Naming n => n -> [B.ByteString] -> IO ()
writeNewWeave naming payload = do
   now <- getCurrentTime
   let delta1 = DeltaInfo {
      name = "newweavename",
      number = 1,
      tags = Map.fromList [("info", "some stuff here")],
      time = now }
   let header = Header {
      version = 1,
      deltas = [delta1] }
   let full = WeaveHeader header : WeaveInsert 1 : map (\x -> WeavePlain x Nothing) payload
         ++ [WeaveEnd 1]
   fullStream <- Streams.fromList full
   tmpName <- withTemp naming False $ \tmpName handle -> do
      output <- Streams.handleToOutputStream handle
      weaveEncode fullStream output
      return tmpName
   let mainName = genName naming (preferredExtension naming) False
   let bakName = genName naming "bak" False
   catch (renameFile mainName bakName) (ignoreIOError ())
   renameFile tmpName mainName

writeNewDelta :: Naming n => n -> [B.ByteString] -> IO ()
writeNewDelta naming payload = do
   let mainName = genName naming (preferredExtension naming) False
   header <- readHeader mainName
   let latestDelta = highestDelta header
   _header2 <- addDelta "weave2" (Map.fromList [("info", "more stuff")]) header
   lastPayload <- readDelta mainName latestDelta
   let diffs = diff lastPayload payload
   diffStream <- Streams.fromList diffs
   withWeaveStream mainName latestDelta $ \input -> do
      output <- genNew latestDelta input diffStream
      fullOut <- Streams.toList output
      putStrLn $ show fullOut

ignoreIOError :: a -> IOError -> IO a
ignoreIOError res _ = return res

-- Augment 'input' with a new delta, based on diffs
genNew :: Int
   -> InputStream WeaveElement
   -> InputStream (Item B.ByteString)
   -> IO (InputStream WeaveElement)
genNew delta input diffs = Streams.fromGenerator $ newGen delta input diffs

newGen :: Int -> InputStream WeaveElement -> InputStream (Item B.ByteString) -> Generator WeaveElement ()
newGen _delta input diffs = do
   numbered <- liftIO $ numberLines diffs
   hdInput1 <- liftIO $ Streams.read input
   hdDiffs1 <- liftIO $ Streams.read numbered
   let
      loop
         :: Int
         -> State
         -> Maybe (Int, (Item B.ByteString))
         -> Maybe WeaveElement
         -> Generator WeaveElement ()

      -- Non 'plain' lines are just passed through
      loop lineNo state ds (Just inp@(WeaveHeader _)) = do
         yield inp
         nextInput <- liftIO $ Streams.read input
         loop lineNo state ds nextInput
      -- We are looking at an old line in the diff, generate up until
      -- that point.
      {-
      loop lineNo state ds@(Just (n1, Old _)) (Just inp@(WeavePlain t (Just n2)))
         | n1 > n2 -> do
            -- Before this point in the input, just feed lines
            -- through.
            yield $ inp
            hdInput' <- liftIO $ Streams.read input
            loop lineNo state ds hdInput'
         | otherwise -> do
            yield $ WeaveDelete delta
            yield $ inp
            loop lineNo state  hdInput'
      -}
      loop lineNo state hdInput hdDiffs = do
         liftIO $ putStrLn $ show (lineNo, state, hdInput, hdDiffs)
         undefined
   loop 0 Idle hdDiffs1 hdInput1
{-
genNew delta input diffs = do
   numbered <- numberLines diffs
   state <- newIORef Idle
   let
      advance = do
         _ <- Streams.read numbered
         return ()
      gen = do
         diffItem <- Streams.peek numbered
         inputItem <- Streams.read diffs
         st <- readIORef state
         case (diffItem, inputItem, st) of
            (Just (n1, Old _), Just (WeavePlain t (Just n2)))
               | n1 < n2 -> do
                  putStrLn $ show inputItem
                  gen
               | otherwise -> do
                  putStrLn $ Just $ WeaveInsert delta
                  putStrLn $ diffItem
                  advance
                  gen
            (Just (_, Old _), Just _) -> do
               putStrLn $ show inputItem
               gen
   gen
-}

data State = Idle | Deleting | Inserting
   deriving Show

-- Number the lines in a patience diff from the perspective of the old
-- diff.
numberLines :: InputStream (Item B.ByteString) -> IO (InputStream (Int, Item B.ByteString))
numberLines input = do
   lineNo <- newIORef 0
   let
      inc elt = do
         modifyIORef lineNo (+ 1)
         cur <- readIORef lineNo
         return $ Just (cur, elt)
      gen = do
         elt <- Streams.read input
         case elt of
            Nothing -> return $ Nothing
            Just (Old a) -> inc $ Old a
            Just (Both a b) -> inc $ Both a b
            Just (New a) -> do
               cur <- readIORef lineNo
               return $ Just (cur, New a)
   Streams.makeInputStream gen

t1 :: IO ()
t1 = do
   let naming = SimpleNaming "haha" "dat" False
   let elt1 = map (C8.pack . show) [1 :: Int .. 100]
   writeNewWeave naming elt1
   let elt2 = map (C8.pack . show) [2 :: Int .. 101]
   writeNewDelta naming elt2

-- |Delta generation for weave files

{-# LANGUAGE OverloadedStrings #-}

module Hack.Weave.Delta (
) where

import Hack.Weave.Header
import Hack.Weave.Naming
import Hack.Weave.Parse
import Hack.Weave.Types
import Hack.Weave.Write

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime)
import Control.Exception (catch)
import System.Directory (renameFile)
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
   tmpName <- withTemp naming False $ \name handle -> do
      output <- Streams.handleToOutputStream handle
      weaveEncode fullStream output
      return name
   let mainName = genName naming (preferredExtension naming) False
   let bakName = genName naming "bak" False
   catch (renameFile mainName bakName) (ignoreIOError ())
   renameFile tmpName mainName

ignoreIOError :: a -> IOError -> IO a
ignoreIOError res _ = return res

t1 = do
   let naming = SimpleNaming "haha" "dat" False
   let elt1 = map (C8.pack . show) [1..100]
   writeNewWeave naming elt1

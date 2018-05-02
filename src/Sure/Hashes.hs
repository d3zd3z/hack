-- |Compute file hashes.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sure.Hashes (
    HashProgress(..),
    estimateHashes,
    treeFiles, needsHash
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), notMember)
import qualified Data.Vector as V
import Pipes
import qualified Pipes.Prelude as PP

-- import Sure.Walk (walk)
import Sure.Types

-- |Fold down a Producer representing a tree, and update a state with
-- the values that need updating.
estimateHashes :: Monad m => Producer SureNode m () -> m HashProgress
estimateHashes = PP.fold (flip updateProgress) hashProgress0 id

updateProgress :: SureNode -> HashProgress -> HashProgress
updateProgress (SureNode _ atts) hp =
    if
        Map.lookup "kind" atts /= Just "file" ||
        Map.member "sha1" atts then hp
    else hp {
        hpTotalFiles = hpTotalFiles hp + 1,
        hpTotalBytes = hpTotalBytes hp + size }
    where
        size :: Integer
        size = maybe 0 id $ fmap (read . B.unpack) $ Map.lookup "size" atts
updateProgress _ hp = hp

data HashProgress = HashProgress {
    hpTotalFiles :: Integer,
    hpTotalBytes :: Integer,
    hpFiles :: Integer,
    hpBytes :: Integer }
    deriving Show

hashProgress0 :: HashProgress
hashProgress0 = HashProgress 0 0 0 0

-- |Estimate the work needed to update hashes.  We consider any file
-- nodes that do not have a 'sha1' field as needing hashes.

-- |Walk through a tree, returning all of the file nodes.
treeFiles :: SureTree -> [SureFile]
treeFiles SureTree{..} =
    concatMap treeFiles (V.toList stChildren) ++
    V.toList stFiles

-- |Does this file node need a hash computed on it.
needsHash :: SureFile -> Bool
needsHash f = atts ! "kind" == "file" && notMember "sha1" atts
    where atts = sfAtts f

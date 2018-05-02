-- |Compute file hashes.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sure.Hashes (
    HashProgress(..),
    estimateHashes
) where

import Pipes
import qualified Pipes.Prelude as PP

-- import Sure.Walk (walk)
import Sure.Types

-- |Fold down a Producer representing a tree, and update a state with
-- the values that need updating.
estimateHashes :: Monad m => Producer SureNode m () -> m HashProgress
estimateHashes = PP.fold (flip updateProgress) hashProgress0 id

updateProgress :: SureNode -> HashProgress -> HashProgress
updateProgress node hp =
    if needsHash node then hp {
        hpTotalFiles = hpTotalFiles hp + 1,
        hpTotalBytes = hpTotalBytes hp + nodeSize node }
    else hp

data HashProgress = HashProgress {
    hpTotalFiles :: Integer,
    hpTotalBytes :: Integer,
    hpFiles :: Integer,
    hpBytes :: Integer }
    deriving Show

hashProgress0 :: HashProgress
hashProgress0 = HashProgress 0 0 0 0

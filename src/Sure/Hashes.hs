-- |Compute file hashes.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sure.Hashes (
    treeFiles, needsHash
) where

import Data.Map.Strict ((!), notMember)
import qualified Data.Vector as V

-- import Sure.Walk (walk)
import Sure.Types

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

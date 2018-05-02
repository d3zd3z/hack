-- | Types for sure files

{-# LANGUAGE OverloadedStrings #-}

module Sure.Types (
   AttMap,
   SureTree(..),
   SureFile(..),
   SureNode(..),

   isFile,
   needsHash,
   nodeSize
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)

-- |An attribute maintains a k/v mapping for the filesystem mappings.
-- Everything is treated as ascii strings, with a special escaping
-- used for things that came from strings that are user specified.
type AttMap = Map B.ByteString B.ByteString

-- |The top of a suretree.
data SureTree = SureTree {
   stName :: !B.ByteString,
   stAtts :: !AttMap,
   stChildren :: !(Vector SureTree),
   stFiles :: !(Vector SureFile) }
   deriving Show

data SureFile = SureFile {
   sfName :: !B.ByteString,
   sfAtts :: !AttMap }
   deriving Show

-- |The tree traversal is a SureNode.
data SureNode =
    SureEnter !B.ByteString !AttMap |
    SureLeave |
    SureSep |
    SureNode !B.ByteString !AttMap
    deriving Show

-- |Indicate if this node represents a regular file.
isFile :: SureNode -> Bool
isFile (SureNode _ atts) = Map.lookup "kind" atts == Just "file"
isFile _                 = False

-- |Determine if this node needs a hash computation.  This is for
-- regular files that don't have a "sha1" property.
needsHash :: SureNode -> Bool
needsHash (SureNode _ atts) =
    Map.lookup "kind" atts == Just "file" &&
        Map.notMember "sha1" atts
needsHash _                 = False

-- |Return the size of this node.  Returns '0' if there is no size
-- property, or if the node isn't a regular node.
nodeSize :: SureNode -> Integer
nodeSize (SureNode _ atts) = maybe 0 id $ fmap (read . C8.unpack) $ Map.lookup "size" atts
nodeSize _                 = 0

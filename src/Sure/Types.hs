-- | Types for sure files

module Sure.Types (
   AttMap,
   SureTree(..),
   SureFile(..),
   SureNode(..)
) where

import qualified Data.ByteString as B
import Data.Map.Strict (Map)
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

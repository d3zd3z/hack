-- |Types representing a weave file.

module Data.Weave.Types (
   WeaveElement(..)
) where

import qualified Data.ByteString as B
import Data.Weave.Header (Header(..))

-- |The stream is decoded into a Stream of the following.  The
-- WeavePlain lines will eventually get line number and 'keep'
-- information associated with them, although this is added later.
data WeaveElement
   = WeaveHeader Header
   | WeaveInsert Int
   | WeaveDelete Int
   | WeaveEnd Int
   | WeavePlain B.ByteString (Maybe Int)
   | WeaveError B.ByteString
   deriving Show


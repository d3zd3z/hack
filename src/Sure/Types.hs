-- | Types for sure files

{-# LANGUAGE OverloadedStrings #-}

module Sure.Types (
   AttMap,
   SureNode(..),

   addDirs,
   updateAtt,

   isFile,
   needsHash,
   nodeSize
) where

import Conduit
import Control.Exception (throwIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))

-- |An attribute maintains a k/v mapping for the filesystem mappings.
-- Everything is treated as ascii strings, with a special escaping
-- used for things that came from strings that are user specified.
type AttMap = Map B.ByteString B.ByteString

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

-- |This pipe adds a directory tracker to the input stream.  The
-- __root__ is replaced with the rootDir argument, but otherwise, the
-- names are from the nodes.
addDirs :: MonadIO m => B.ByteString -> ConduitT SureNode (B.ByteString, SureNode) m ()
addDirs rootDir = do
    node <- await
    case node of
        Just nn@(SureEnter name _)
            | name == "__root__" -> do
                yield $ (rootDir, nn)
                addDirs' rootDir
            | otherwise          -> do
                liftIO $ throwIO $ userError $ "Root directory not __root__: " ++ show name
        Nothing -> return ()
        _ -> liftIO $ throwIO $ userError $ "Node tree does not start with \"Enter\" node"

-- Helper, walks a directory, after seeing the SureEnter (and yielding
-- it).  Will return after seeing and yielding the SureLeave.
addDirs' :: Monad m => B.ByteString -> ConduitT SureNode (B.ByteString, SureNode) m ()
addDirs' dir = do
    node <- await
    case node of
        Just nn@(SureEnter name _) -> do
            let subName = dir <> "/" <> name
            yield (subName, nn)
            addDirs' subName  -- Nested recursion.
            addDirs' dir      -- Tail call to continue.
        Just SureLeave -> yield (dir, SureLeave)
        Just SureSep -> yield (dir, SureSep) >> addDirs' dir
        Just nn@(SureNode name _) -> do
            let subName = dir <> "/" <> name
            yield (subName, nn)
            addDirs' dir
        Nothing -> return ()

-- Attempt to update the `key` attribute to the given value.
updateAtt :: SureNode -> B.ByteString -> B.ByteString -> SureNode
updateAtt (SureNode name atts) key value = SureNode name $ Map.insert key value atts
updateAtt (SureEnter name atts) key value = SureEnter name $ Map.insert key value atts
updateAtt node _ _ = node

-- |Signing off
--
-- The signoff compares two trees, and generates a report of what has
-- changed between them.  The report can be rendered textually if
-- desired.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Sure.Signoff (
    ReportNode(..),
    prettyNode,
    signoff
) where

import Conduit
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Text.Printf (printf)

import Sure.Compare
import Sure.Types

data ReportNode
    = AddItem B.ByteString B.ByteString
    -- ^ path of entity, and it's type ("dir", "file").
    | DelItem B.ByteString B.ByteString
    -- ^ path of entity, and it's type ("dir", "file").
    | ChangeItem B.ByteString [B.ByteString]
    -- ^ path of entity, what attributes have changed.
    deriving Show

-- TODO: Do this without using String
prettyNode :: ReportNode -> String
prettyNode (AddItem name kind) = printf "+ %-22s %s" (B.unpack kind) (B.unpack name)
prettyNode (DelItem name kind) = printf "- %-22s %s" (B.unpack kind) (B.unpack name)
prettyNode (ChangeItem name atts) =
    printf "  [%-20s] %s" (B.unpack $ B.intercalate "," atts) (B.unpack name)

-- | Process the combo nodes from 'combineTrees' and generate the
-- report output.
signoff :: Monad m => ConduitT (ComboNode SureNode) ReportNode m ()
signoff = loop emptyDirTrack where
    loop dt = do
        mnode <- await
        case mnode of
            Nothing -> return ()
            Just (First (SureEnter name _)) -> do
                yield $ DelItem (makeName dt name) "dir"
                eatDir ofFirst
                loop dt
            Just (Second (SureEnter name _)) -> do
                yield $ AddItem (makeName dt name) "dir"
                eatDir ofSecond
            Just (First (SureNode name _)) -> do
                yield $ DelItem (makeName dt name) "file"
                loop dt
            Just (Second (SureNode name _)) -> do
                yield $ AddItem (makeName dt name) "file"
                loop dt
            Just (First _) -> error "Unexpected left node"
            Just (Second _) -> error "Unexpected right node"
            Just (Both (SureEnter _ att1) (SureEnter name att2)) -> do
                case attCmp att1 att2 of
                    [] -> return ()
                    atts -> yield $ ChangeItem (makeName dt name) atts
                loop (pushDir name dt)
            Just (Both (SureNode _ att1) (SureNode name att2)) -> do
                case attCmp att1 att2 of
                    [] -> return ()
                    atts -> yield $ ChangeItem (makeName dt name) atts
                loop dt
            Just (Both SureSep SureSep) -> loop dt
            Just (Both SureLeave SureLeave) -> do
                loop (popDir dt)
            _ -> error $ show mnode

ofFirst :: ComboNode SureNode -> Maybe SureNode
ofFirst (First node) = Just node
ofFirst _            = Nothing

ofSecond :: ComboNode SureNode -> Maybe SureNode
ofSecond (Second node) = Just node
ofSecond _             = Nothing

eatDir :: Monad m => (ComboNode SureNode -> Maybe SureNode) -> ConduitT (ComboNode SureNode) b m ()
eatDir valid = do
    mnode <- fmap valid <$> await
    case mnode of
        Just Nothing -> error "Unexpected node"
        Just (Just (SureEnter _ _)) -> do
            eatDir valid
            eatDir valid
        Just (Just SureLeave) -> return ()
        Just (Just _) -> eatDir valid
        Nothing -> error "Unexpected end of stream"

data DirTrack = DirTrack {
    dtDirs :: [B.ByteString],
    dtPath :: B.ByteString }

emptyDirTrack :: DirTrack
emptyDirTrack = DirTrack [] ""

pushDir :: B.ByteString -> DirTrack -> DirTrack
pushDir name DirTrack{..} =
    let full = name:dtDirs in
    DirTrack full (B.intercalate "/" $ safeTail $ reverse full)

popDir :: DirTrack -> DirTrack
popDir (DirTrack (_:xs) _) = DirTrack xs (B.intercalate "/" $ safeTail $ reverse xs)
popDir _ = error "more SureLeave than SureEnter"

makeName :: DirTrack -> B.ByteString -> B.ByteString
makeName (DirTrack [] _) name = name
makeName (DirTrack [_] _) name = name
makeName DirTrack{dtPath} name = dtPath <> "/" <> name

-- Compare the attributes.  For now, we only compare attributes
-- present in both.
-- TODO: Generate a warning about key variations.
attCmp :: AttMap -> AttMap -> [B.ByteString]
attCmp aa bb = Map.keys $ Map.filter id $ Map.intersectionWith (/=) (clean aa) (clean bb)
    where clean = Map.delete "ctime" . Map.delete "ino"

safeTail :: [a] -> [a]
safeTail [] = []
safeTail a = tail a

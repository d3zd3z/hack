-- |Compute file hashes.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sure.Hashes (
    HashProgress(..),
    estimateHashes,
    computeHashes,
    combineHashes
) where

import Conduit
import Control.Concurrent (newMVar, modifyMVar_)
import qualified Crypto.Hash as H
import qualified Data.ByteArray as DBA
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified System.Posix.ByteString as U
import Text.Human
import Text.Progress
import Text.Printf (printf)

import Control.Concurrent.ParMap
import Sure.Compare
import Sure.Walk (safely)
import Sure.Types

-- |Fold down a Producer representing a tree, and update a state with
-- the values that need updating.
estimateHashes :: Monad m => ConduitT SureNode o m HashProgress
estimateHashes = foldlC (flip updateProgress) mempty

updateProgress :: SureNode -> HashProgress -> HashProgress
updateProgress node hp =
    if needsHash node then hp <> HashProgress 1 (nodeSize node)
        else hp

-- |The progress of hash updating.  Tracks the number of files and the
-- number of bytes visited by a hash update.
data HashProgress = HashProgress {
    hpFiles :: !Integer,
    hpBytes :: !Integer }
    deriving Show

instance Monoid HashProgress where
    mempty = HashProgress 0 0
    mappend (HashProgress a1 b1) (HashProgress a2 b2) = HashProgress (a1 + a2) (b1 + b2)

-- * Hash combining

-- |Walk through two streams together, yielding the 'srcNew' values,
-- but checking for items in the 'old' stream with hashes that might
-- still be valid.
combineHashes :: MonadIO m => ConduitT (ComboNode SureNode) SureNode m ()
combineHashes =
    -- First version, just pass new through.
    awaitForever $ \nd -> do
        case nd of
            Both (SureNode _ att1) nn@(SureNode name2 att2) ->
                if all (\att -> Map.lookup att att1 == Map.lookup att att2)
                    ["mtime", "ctime", "ino", "size"] &&
                   Map.member "sha1" att1
                then
                    yield $ SureNode name2
                        (Map.insert "sha1" (att1 Map.! "sha1") att2)
                else
                    yield nn
            Both __ bb -> yield bb
            Second nn -> yield nn
            First _ -> return ()

-- * Hash updating
--
-- | Compute the hashes for the given stream.
computeHashes
    :: MonadIO m
    => PMeter
    -> HashProgress
    -> ConduitT (B.ByteString, SureNode) SureNode m ()
computeHashes meter total = do
    mv <- liftIO $ newMVar (0 :: Int)
    let
        update1 (name, node) = do
            if needsHash node then do
                modifyMVar_ mv $ \level -> do
                    putStrLn $ (replicate (3 * level) ' ') ++ "Hashing: " ++ show name
                    return $ level + 1
                hash <- hashFile name
                modifyMVar_ mv $ \level -> do
                    putStrLn $ (replicate (3 * (level-1)) ' ') ++ "Done: " ++ show name
                    return $ level - 1
                let hp' = updateProgress node mempty  -- TODO: Make more moniodal
                return (hp', maybe node (updateAtt node "sha1") hash)
            else return (mempty, node)
    _ <- parMapAccumM update1 (\hp -> showStatus meter hp total) mempty
    return ()
{-
computeHashes meter total = CL.mapAccumM process mempty >> return ()
    where
        process :: MonadIO mm
            => (B.ByteString, SureNode)
            -> HashProgress
            -> mm (HashProgress, SureNode)
        process (name, node) hp = do
            if needsHash node then do
                hash <- liftIO $ hashFile name
                let hp' = updateProgress node hp
                liftIO $ showStatus meter hp' total
                return (hp', maybe node (updateAtt node "sha1") hash)
            else return (hp, node)
-}

showStatus :: PMeter -> HashProgress -> HashProgress -> IO ()
showStatus meter hp total = do
    pmPut meter $ printf "%8d/%8d (%5.1f%%) files, %s/%s (%5.1f%%) bytes"
        (hpFiles hp) (hpFiles total)
        ((fromIntegral $ hpFiles hp :: Double) /
         (fromIntegral $ hpFiles total) * 100.0)
        (humanizeBytes $ hpBytes hp)
        (humanizeBytes $ hpBytes total)
        ((fromIntegral $ hpBytes hp :: Double) /
         (fromIntegral $ hpBytes total) * 100.0)

-- | Compute the hash of a given file, returning it if possible.
-- To preserve compatibility with the rest of this system, use the
-- Posix.ByteString operations on the file.  It is lower-level, and
-- more complex, but will avoid problems with filenames that have
-- invalid Unicode characters in them.
--
-- TODO: Although many people run without atime enabled, Linux does
-- provide a way of disabling the atime change upon read.  This
-- doesn't appear to be visible in the 'unix' package, so would have
-- to be bound manually.
hashFile :: B.ByteString -> IO (Maybe B.ByteString)
hashFile = safely . hashFile'

hashFile' :: B.ByteString -> IO B.ByteString
hashFile' name = do
    fd <- U.openFd name U.ReadOnly Nothing U.defaultFileFlags
    h <- U.fdToHandle fd
    payload <- L.hGetContents h
    let hash = H.hashlazy payload :: H.Digest H.SHA1
    return $ hexifyB hash

-- A builder that converts a ByteArray unto its hex representation.
hexifyB :: DBA.ByteArrayAccess b => b -> B.ByteString
hexifyB =
    L.toStrict .
        B.toLazyByteString .
        mconcat .
        map B.word8HexFixed .
        DBA.unpack

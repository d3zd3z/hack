-- |Compute file hashes.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sure.Hashes (
    HashProgress(..),
    estimateHashes,
    computeHashes,
    combineHashes
) where

import qualified Crypto.Hash as H
import qualified Data.ByteArray as DBA
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid ((<>))
import Pipes
import qualified Pipes.Prelude as PP
import qualified System.Posix.ByteString as U
import Text.Human
import Text.Progress
import Text.Printf (printf)

import Sure.Walk (safely)
import Sure.Types

-- |Fold down a Producer representing a tree, and update a state with
-- the values that need updating.
estimateHashes :: Monad m => Producer SureNode m () -> m HashProgress
estimateHashes = PP.fold (flip updateProgress) mempty id

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
combineHashes
    :: Monad m
    => Producer SureNode m ()
    -> Producer SureNode m ()
    -> Producer SureNode m ()
combineHashes _srcOld srcNew = do
    -- old <- lift $ next srcOld
    new <- lift $ next srcNew
    -- unless (sameDir old new) $ lift $ throwIO $ userException "directory mismatch"
    loop new
    where
        -- Process the top level directory.  old and new should be the
        -- first nodes within each directory.  May call itself
        -- recursively to process subdirectories, and will talk-call
        -- to handle the file processing nodes at the end.
        loop
            :: Monad mm
            => Either () (SureNode, Producer SureNode mm ())
            -> Producer SureNode mm ()
        loop (Left r) = return r
        loop (Right (node, src2)) = do
            yield node
            hd <- lift $ next src2
            loop hd

        {-
        -- Most of the time, end of input is an error, so to simplify,
        -- raise an exception if we get to the end when unexpected
        mustNext :: MonadIO m => Producer SureNode m () -> Producer SureNode m SureNode
        mustNext src = do
            node <- lift $ next src
            either
                (\r -> lift $ throwIO $ userException $ "Unexpected exception" ++ show r)
                id
                node
        -}

-- * Hash updating
--
-- | Compute the hashes for the given stream.
computeHashes
    :: MonadIO m 
    => PMeter
    -> HashProgress
    -> Pipe (B.ByteString, SureNode) SureNode m ()
computeHashes meter total = do
    let
        loop :: MonadIO mm => HashProgress -> Pipe (B.ByteString, SureNode) SureNode mm ()
        loop hp = do
            (name, node) <- await
            if needsHash node then do
                hash <- liftIO $ hashFile name
                yield $ maybe node (updateAtt node "sha1") hash
                let hp' = updateProgress node hp
                liftIO $ showStatus meter hp' total
                loop hp'
            else do
                yield node
                loop hp
    loop mempty

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

-- |Internals of hash computation

module Sure.Hashes.Internal (
    hashFile,
    HashProgress(..),
    updateProgress,
    showStatus
) where

import qualified Crypto.Hash as H
import qualified Data.ByteArray as DBA
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid ((<>))
import qualified System.Posix.ByteString as U
import Text.Printf (printf)

import Sure.Types (SureNode, needsHash, nodeSize)
import Sure.Walk (safely)
import Text.Human (humanizeBytes)
import Text.Progress (PMeter, pmPut)

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

-- *
-- |Hash progress indicator.
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

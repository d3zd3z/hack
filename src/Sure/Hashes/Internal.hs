-- |Internals of hash computation

module Sure.Hashes.Internal (
    hashFile
) where

import qualified Crypto.Hash as H
import qualified Data.ByteArray as DBA
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified System.Posix.ByteString as U

import Sure.Walk (safely)

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

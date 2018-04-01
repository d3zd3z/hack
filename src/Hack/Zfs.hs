-- ZFS operations
module Hack.Zfs (
   ZfsInfo,
   ZfsEntry(..),
   getZfs,
   getByName,
   zfsEntries
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.List (groupBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.Process.Typed (proc, readProcess_)

-- |A ZFS system can either consist of a plain name, or have a '@'
-- character that indicates a snapshot, or a '#' to indicate a
-- bookmark.
data SnapName =
   None |
   Snapshot B.ByteString |
   Bookmark B.ByteString
   deriving Show

-- |All of the zfs volumes.  It is the entries, in a map.
newtype ZfsInfo = ZfsInfo { byName :: Map B.ByteString ZfsEntry }

-- |Information about a single ZFS volume.
data ZfsEntry = ZfsEntry {
   zName :: B.ByteString,
   zMount :: B.ByteString,
   zSnaps :: [B.ByteString],
   zBooks :: [B.ByteString] }
   deriving Show

-- |Get all filesystems from zfs, along with snapshots and bookmarks.
getZfs :: IO ZfsInfo
getZfs = do
   -- TODO: Check stderr for messages
   (l, _) <- readProcess_ $ proc "zfs" ["list", "-H", "-t", "all", "-o", "name,mountpoint"]
   return $
      ZfsInfo $ Map.fromList $ map (\ent -> (zName ent, ent)) $
      map collapse $ groupBy sameName $ map (unLine . C.split '\t' . L.toStrict) $ LC.lines l

-- |Lookup an entry, by name.
getByName :: B.ByteString -> ZfsInfo -> Maybe ZfsEntry
getByName key = Map.lookup key . byName

-- |Return all of the entries.
zfsEntries :: ZfsInfo -> [ZfsEntry]
zfsEntries = map snd . Map.toList . byName

-- |Decode a name from ZFS as a SnapName, returning the base
-- filesystem name, and the potential snapshot name.
decodeName :: B.ByteString -> (B.ByteString, SnapName)
decodeName name =
   case breakTo '@' name of
      Just (a, b) -> (a, Snapshot b)
      Nothing -> case breakTo '#' name of
         Just (a, b) -> (a, Bookmark b)
         Nothing -> (name, None)

-- |Stop at the first instance of the given character, returning Just
-- (before, after) if the character is present, or Nothing if it is
-- not.
breakTo :: Char -> B.ByteString -> Maybe (B.ByteString, B.ByteString)
breakTo ch bs =
   let (a, b) = C.break (== ch) bs in
   case B.uncons b of
      Nothing -> Nothing
      Just (_, b') -> Just (a, b')

type OneEntry = (B.ByteString, SnapName, B.ByteString)

-- |Decompose the snapname, and return it.  This shouldn't fail, so
-- will throw an error on invalid lines.
unLine :: [B.ByteString] -> OneEntry
unLine [name, mount] =
   let (n, sn) = decodeName name in
   (n, sn, mount)
unLine _ = error "Invalid number of fields from zfs list"

-- |Are these two entries, the same name?
sameName :: (B.ByteString, a, b) -> (B.ByteString, a, b) -> Bool
sameName (a1, _, _) (a2, _, _) =  a1 == a2

-- |Given a list of triplets where we assume the name is the same, make
-- these into an ZfsEntry
collapse :: [OneEntry] -> ZfsEntry
collapse aa@((name, _, mount) : _) =
   ZfsEntry {
      zName = name,
      zMount = mount,
      zSnaps = snaps aa,
      zBooks = books aa }
   where
      snaps :: [OneEntry] -> [B.ByteString]
      snaps [] = []
      snaps ((_, Snapshot n, _) : xs) = n : snaps xs
      snaps (_ : xs) = snaps xs

      books :: [OneEntry] -> [B.ByteString]
      books [] = []
      books ((_, Bookmark n, _) : xs) = n : books xs
      books (_ : xs) = books xs
collapse [] = error "Unexpected empty list"

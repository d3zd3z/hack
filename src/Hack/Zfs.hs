-- ZFS operations
module Hack.Zfs (
   ZfsEntry(..),
   getZfs
) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (groupBy)
import System.Process.Typed (proc, readProcess_)

-- |A ZFS system can either consist of a plain name, or have a '@'
-- character that indicates a snapshot, or a '#' to indicate a
-- bookmark.
data SnapName =
   None |
   Snapshot ByteString |
   Bookmark ByteString
   deriving Show

data ZfsEntry = ZfsEntry {
   zName :: ByteString,
   zMount :: ByteString,
   zSnaps :: [ByteString],
   zBooks :: [ByteString] }
   deriving Show

-- |Decode a name from ZFS as a SnapName, returning the base
-- filesystem name, and the potential snapshot name.
decodeName :: ByteString -> (ByteString, SnapName)
decodeName name =
   case breakTo '@' name of
      Just (a, b) -> (a, Snapshot b)
      Nothing -> case breakTo '#' name of
         Just (a, b) -> (a, Bookmark b)
         Nothing -> (name, None)

-- |Stop at the first instance of the given character, returning Just
-- (before, after) if the character is present, or Nothing if it is
-- not.
breakTo :: Char -> ByteString -> Maybe (ByteString, ByteString)
breakTo ch bs =
   let (a, b) = C.break (== ch) bs in
   case C.uncons b of
      Nothing -> Nothing
      Just (_, b') -> Just (a, b')

type OneEntry = (ByteString, SnapName, ByteString)

-- |Decompose the snapname, and return it.  This shouldn't fail, so
-- will throw an error on invalid lines.
unLine :: [ByteString] -> (ByteString, SnapName, ByteString)
unLine [name, mount] =
   let (n, sn) = decodeName name in
   (n, sn, mount)
unLine _ = error "Invalid number of fields from zfs list"

-- |Are these two entries, the same name?
sameName :: (ByteString, a, b) -> (ByteString, a, b) -> Bool
sameName (a1, _, _) (a2, _, _) =  a1 == a2

-- |Given a list of triplets where we assume the name is the same, make
-- these into an ZfsEntry
collapse :: [(ByteString, SnapName, ByteString)] -> ZfsEntry
collapse aa@((name, _, mount) : _) =
   ZfsEntry {
      zName = name,
      zMount = mount,
      zSnaps = snaps aa,
      zBooks = books aa }
   where
      snaps :: [OneEntry] -> [ByteString]
      snaps [] = []
      snaps ((_, Snapshot n, _) : xs) = n : snaps xs
      snaps (_ : xs) = snaps xs

      books :: [OneEntry] -> [ByteString]
      books [] = []
      books ((_, Bookmark n, _) : xs) = n : books xs
      books (_ : xs) = books xs
collapse [] = error "Unexpected empty list"

-- |Get all filesystems from zfs, along with snapshots and bookmarks.
getZfs :: IO [ZfsEntry]
getZfs = do
   -- TODO: Check stderr for messages
   (l, _) <- readProcess_ $ proc "zfs" ["list", "-H", "-t", "all", "-o", "name,mountpoint"]
   return $ map collapse $ groupBy sameName $ map (unLine . C.split '\t') $ C.lines l

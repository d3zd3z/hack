-- |File Attribute management

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Sure.Atts (
   AttMap,
   getAtts,
   isDir
) where

import Control.Exception (tryJust)
import Control.Monad (guard)
import Data.Bits ((.&.), complement, shiftR)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.IO.Error (isUserError)
import qualified System.Posix.ByteString as P

-- |An attribute maintains a k/v mapping for the filesystem mappings.
-- Everything is treated as ascii strings, with a special escaping
-- used for things that came from strings that are user specified.
type AttMap = Map B.ByteString B.ByteString

type Converter = P.FileStatus -> IO (B.ByteString, B.ByteString)

getOwner :: Converter
getOwner = return . ("uid",) . B.pack . show . P.fileOwner

getGroup :: Converter
getGroup = return . ("gid",) . B.pack . show . P.fileGroup

getPerm :: Converter
getPerm = return . ("perm",) . B.pack . show . (.&. complement P.fileTypeModes) . P.fileMode

getIno :: Converter
getIno = return . ("ino",) . B.pack . show . P.fileID

getSize :: Converter
getSize = return . ("size",) . B.pack . show . P.fileSize

-- TODO: rsure, only returns seconds, which is convenient as 'unix'
-- only returns that as well.
getMtime :: Converter
getMtime = return . ("mtime",) . B.pack . show . P.modificationTime

getCtime :: Converter
getCtime = return . ("ctime",) . B.pack . show . P.statusChangeTime

getDevMaj :: Converter
getDevMaj = return . ("devmaj",) . B.pack . show . (.&. 0xfff) . (`shiftR` 8) . P.specialDeviceID

getDevMin :: Converter
getDevMin = return . ("devmin",) . B.pack . show . (.&. 0xff) . P.specialDeviceID

getBase :: [Converter]
getBase = [getOwner, getGroup, getPerm]

getDev :: [Converter]
getDev = [getDevMaj, getDevMin]

-- Extract the attributes from the FileStatus into the generalized
-- map.  Note that for regular files, the sha1 attribute is not
-- generated, as that will be computed later.  The name is given so
-- that symlinks can be read.
getAtts :: P.RawFilePath -> P.FileStatus -> IO AttMap
getAtts path stat
   | P.isBlockDevice stat = gen "blk" $ getBase ++ getDev
   | P.isCharacterDevice stat = gen "chr" $ getBase ++ getDev
   | P.isNamedPipe stat = gen "fifo" $ getBase
   | P.isRegularFile stat = gen "file" $ getBase ++ [getIno, getSize, getMtime, getCtime]
   | P.isDirectory stat = gen "dir" $ getBase
   | P.isSymbolicLink stat = gen "lnk" $ getBase ++ [getTarget path]
   | P.isSocket stat = gen "sock" $ getBase
   | otherwise = error "Unknown file type"
   where
      gen :: B.ByteString -> [Converter] -> IO AttMap
      gen kind conv = do
         atts <- mapM (\f -> f stat) conv
         return $ Map.fromList $ ("kind", kind) : atts

-- Attempt to read a symlink, log if unable to (and return just
-- "???").
getTarget :: P.RawFilePath -> a -> IO (B.ByteString, B.ByteString)
getTarget path _ = do
   r <- tryJust (guard . not . isUserError) $ P.readSymbolicLink path
   return $ ("targ", either (const "???") id r)

-- An example of calling this.
-- @
--   getMany :: [P.RawFilePath] -> IO [(P.RawFilePath, AttMap)]
--   getMany =
--      mapM $ \path -> do
--         stat <- P.getSymbolicLinkStatus path -- TODO: Safely
--         atts <- getAtts path stat
--         return (path, atts)
-- @

-- |Query if this attribyte identifies a directory.
isDir :: AttMap -> Bool
isDir = (== Just "dir") . Map.lookup "kind"

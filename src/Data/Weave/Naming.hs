-- |Weave file naming conventions

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Weave.Naming (
   Naming(..),
   SimpleNaming(..),
   parseNaming,
   openTemp,
   withTemp,
   openPrimary
) where

import Conduit
import Control.Exception (throwIO, try)
import Control.Monad.Catch (bracket, MonadMask)
import qualified Data.ByteString as B
import Data.Conduit.Zlib (ungzip)
import System.IO (Handle, hClose, IOMode(..), openFile)
import System.IO.Error (IOError)
import System.IO.SafeIO (openExclusiveWrite)

import Data.Weave.Naming.Internal
import Data.Weave.Naming.Parse

-- | 'openTemp' @naming comp@ opens a new file using the given naming
-- convention, with possible compression.  Will return a name that
-- doesn't exist yet, and the file will be opened.  Returns the newly
-- opened name, and an open-for-write handle.
openTemp :: Naming n => n -> Bool -> IO (FilePath, Handle)
openTemp naming comp = loop 1
   where
      loop :: Int -> IO (FilePath, Handle)
      loop n = do
         if n > 200 then throwIO $ userError $ "Unable to create temp file: " ++ show naming
         else do
            let name = genName naming (show n) comp
            efd <- try $ openExclusiveWrite name
            either (\(_ :: IOError) -> loop (n + 1))
               (\fd -> return (name, fd))
               efd

-- |Construct a source of lines from the primary file.
openPrimary
    :: (Naming n, MonadResource m, PrimMonad m, MonadThrow m)
    => n
    -> ConduitT () B.ByteString m ()
openPrimary n = do
    let src = sourceIOHandle $ openFile
            (genName n (preferredExtension n) (isNameCompressed n))
            ReadMode
    let unc = if (isNameCompressed n) then src .| ungzip else src
    unc .| linesUnboundedAsciiC

-- | 'withTemp' @naming comp action@ invokes action with a newly
-- created temp file.
withTemp :: (Naming n, MonadMask m, MonadIO m) => n -> Bool -> (FilePath -> Handle -> m a) -> m a
withTemp naming comp = bracket (liftIO $ openTemp naming comp) (liftIO . hClose . snd) . uncurry

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
import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import Text.Progress

import Control.Concurrent.ParMap
import Sure.Compare
import Sure.Hashes.Internal
import Sure.Types

-- |Fold down a Producer representing a tree, and update a state with
-- the values that need updating.
estimateHashes :: Monad m => ConduitT SureNode o m HashProgress
estimateHashes = foldlC (flip updateProgress) mempty

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
--
-- This uses parMapAccumM to try and do more work in parallel.  In
-- practice, this doesn't do very well, since it will only have a
-- certain number of work items queued, and then it waits for the slow
-- ones to finish.  Large files tend to get "stuck" blocking smaller
-- items from causing more work to queue up.  It is difficult to do
-- more work without risking using potentially unbounded amounts of
-- memory to hold intermediate work.
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
                when chatty $ modifyMVar_ mv $ \level -> do
                    putStrLn $ (replicate (3 * level) ' ') ++ "Hashing: " ++ show name
                    return $ level + 1
                hash <- hashFile name
                when chatty $ modifyMVar_ mv $ \level -> do
                    putStrLn $ (replicate (3 * (level-1)) ' ') ++ "Done: " ++ show name
                    return $ level - 1
                let hp' = updateProgress node mempty  -- TODO: Make more moniodal
                return (hp', maybe node (updateAtt node "sha1") hash)
            else return (mempty, node)
    _ <- parMapAccumM update1 (\hp -> showStatus meter hp total) mempty
    return ()

-- | A faster hash update.  In order to be able to update hashes, and
-- saturate the CPUs, we need to be able to compute hashes in an
-- arbitrary order, potentially reording things before generating the
-- final result.  We will need to have some limit on how much we
-- run ahead, so that we don't use too much memory.

chatty :: Bool
chatty = False
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

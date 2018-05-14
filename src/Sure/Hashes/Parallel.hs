-- |Parallel hash computation

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sure.Hashes.Parallel (
    hashSink,
    hashSource,
    numberNodes,
    mergeNodeDb
) where

import Conduit
import Control.Monad (when)
import Control.Monad.Trans.Resource (liftResourceT, register)
import qualified Data.ByteString as B
import Database.HDBC (disconnect, run, toSql, commit, prepare, execute, fetchRow, fromSql)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import System.Directory (removeFile)
import System.IO (hClose)

import Data.Weave.Naming (Naming, openTemp)
import Sure.Hashes.Internal
import Sure.Types
import Text.Progress

-- type NameNode = (B.ByteString, SureNode)
type IndexNameNode = (Int, (B.ByteString, SureNode))

-- |Compute hashes for the streamed nodes, this consumer will write
-- all of the nodes to a database.
hashSink
    :: forall n m. (Naming n, MonadResource m)
    => n
    -> PMeter
    -> HashProgress
    -> ConduitT IndexNameNode Void m FilePath
hashSink n meter total =
    bracketP (toDb n) (disconnect . snd) $ \(tname, conn) -> do
        _ <- liftResourceT $ register $ removeFile tname
        let
            loop :: HashProgress -> ConduitT IndexNameNode Void m ()
            loop hp = do
                xx <- await
                case xx of
                    Nothing -> return ()
                    Just (idx, (path, node)) -> do
                        when (needsHash node) $ do
                            h <- liftIO $ hashFile path
                            -- TODO: Make a MaybeT wrapper here?
                            case h of
                                Nothing -> return ()
                                Just hash -> do
                                    _ <- liftIO $ run conn "INSERT INTO nodes VALUES (?, ?)"
                                        [toSql idx, toSql hash]
                                    return ()
                        let hp' = updateProgress node hp
                        liftIO $ showStatus meter hp total
                        loop hp'
        loop mempty
        liftIO $ commit conn
        return tname

-- |Source the hashes from the given named database.  The query will
-- be in numeric order.
hashSource :: MonadResource m => FilePath -> ConduitT () (Int, B.ByteString) m ()
hashSource tname =
    bracketP (connectSqlite3 tname) disconnect $ \conn -> do
        stmt <- liftIO $ prepare conn $ "SELECT idx, hash FROM nodes ORDER by idx"
        _ <- liftIO $ execute stmt []
        let
            loop = do
                row <- liftIO $ fetchRow stmt
                case row of
                    Nothing -> return ()
                    Just [sIdx, sHash] -> do
                        yield (fromSql sIdx, fromSql sHash)
                        loop
                    Just _ -> error "Invalid SQL response"
        loop

-- |Take two sources, one of SureNode/number pairs, and other of the
-- stream from the database, and incorporate any hashes we can include
-- in the new data.
mergeNodeDb
    :: forall m. Monad m
    => ConduitT () (Int, SureNode) m ()
    -> ConduitT () (Int, B.ByteString) m ()
    -> ConduitT () SureNode m ()
mergeNodeDb tree0 hashes0 = do
    (tree1, tn1) <- lift $ tree0 $$+ await
    (hash1, hn1) <- lift $ hashes0 $$+ await
    let
        loop
            :: SealedConduitT () (Int, SureNode) m ()
            -> Maybe (Int, SureNode)
            -> SealedConduitT () (Int, B.ByteString) m ()
            -> Maybe (Int, B.ByteString)
            -> ConduitT () SureNode m ()
        loop tree treeNode hashes hashNode = do
            case (treeNode, hashNode) of
                (Nothing, _) -> return ()
                (Just (_, node), Nothing) -> do
                    yield node
                    (tree2, tn2) <- lift $ tree $$++ await
                    loop tree2 tn2 hashes hashNode
                (Just (nidx, node), Just (hidx, hash))
                    | nidx == hidx -> do
                        yield $ updateAtt node "sha1" hash
                        (tree2, tn2) <- lift $ tree $$++ await
                        (hash2, hn2) <- lift $ hashes $$++ await
                        loop tree2 tn2 hash2 hn2
                    | nidx < hidx -> do
                        yield node
                        (tree2, tn2) <- lift $ tree $$++ await
                        loop tree2 tn2 hashes hashNode
                    | otherwise -> do
                        (hash2, hn2) <- lift $ hashes $$++ await
                        loop tree treeNode hash2 hn2
    loop tree1 tn1 hash1 hn1

-- Open a temp file, and then open an Sqlite3 database associated with
-- it.  This relies on an empty file being a valid Sqlite3 database.
-- Returns the connection.
toDb :: Naming n => n -> IO (FilePath, Connection)
toDb n = do
    (tname, fd) <- openTemp n False
    hClose fd
    conn <- connectSqlite3 tname
    _ <- run conn "CREATE TABLE nodes (idx INTEGER PRIMARY KEY, hash TEXT)" []
    return (tname, conn)

-- |Number all of the nodes in a conduit.
numberNodes :: Monad m => ConduitT a (Int, a) m ()
numberNodes = loop 1 where
    loop n = do
        item <- await
        case item of
            Nothing -> return ()
            Just a -> do
                yield (n, a)
                loop (n+1)

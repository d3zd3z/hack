-- |Sure tree comparisons.
--
-- Essential to several operations is the comparison of trees.  Since
-- the trees are streamed through Pipes, it takes special care to
-- compare them.  This module contains 'twoTrees', which takes an
-- "old" and a "new" tree, and calls various attachments across the
-- two trees.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Sure.Compare (
    ComboNode(..),
    combineTrees
) where

import Conduit
import Control.Lens
import Control.Monad.Trans.State.Strict

import Sure.Types

-- |A combination node from walking two trees.  This is basically a
-- 3-way Either
data ComboNode a
    = First a
    | Second a
    | Both a a
    deriving (Show)

-- |The guts of tree comparison is this combinator that combines two
-- conduits of tree walks, and returns a combined walk, passing
-- through any nodes that match.  It lives in MonadIO so that it can
-- throw exceptions if the trees are corrupt.  This will recurse over
-- multiple directories until the streams are exhausted, or the
-- SureSep nodes have been reached.
--
-- This algorithm only works if all directories and files are sorted
-- by 'Ord' on 'ByteString'
combineTrees
    :: Monad m
    => ConduitT () SureNode m ()
    -> ConduitT () SureNode m ()
    -> ConduitT () (ComboNode SureNode) m ()
combineTrees first second = do
    (first', fNode) <- lift $ first $$+ await
    (second', sNode) <- lift $ second $$+ await
    let s0 = ((first', fNode), (second', sNode))
    evalStateC s0 combineDirs

-- To make things easier, we keep here the streams, as well as a few
-- into the node at the head of each stream.
-- This is a pair for First and Second, and within each pair is the
-- state pair of the (conduit, node) for the lookahead.
type CombState m = (OneState m, OneState m)
type OneState m = (SealedConduitT () SureNode m (), Maybe SureNode)

firstNode :: Lens' (CombState m) (Maybe SureNode)
firstNode = _1 . _2

secondNode :: Lens' (CombState m) (Maybe SureNode)
secondNode = _2 . _2

type CombStateT m = StateT (CombState m) m

-- Processes a directory.  Will iterate over however many directories
-- there are at this level, returning after consuming the SureSep
-- nodes, or exhausing both streams.
combineDirs :: Monad m => ConduitT () (ComboNode SureNode) (CombStateT m) ()
combineDirs = do
    st0 <- lift get
    case (view firstNode st0, view secondNode st0) of
        (Just fnn@(SureEnter fname _), Just snn@(SureEnter sname _))
            | fname == sname -> do
                -- Matching directories
                yield $ Both fnn snn

                -- Walk the children
                advanceBoth
                combineDirs -- non-tail call.

                -- And continue with this level of dir.
                combineDirs
            | fname < sname -> do
                -- First has a directory second doesn't.
                yield $ First fnn
                advance _1
                passDirs _1 First
                combineDirs
            | otherwise -> do
                -- Second has a directory first doesn't.
                yield $ Second snn
                advance _2
                passDirs _2 Second
                combineDirs
        (Just fnn@(SureEnter _ _), Just SureSep) -> do
            -- First has more directories at the end.
            yield $ First fnn
            advance _1
            passDirs _1 First
            combineDirs
        (Just SureSep, Just snn@(SureEnter _ _)) -> do
            -- Second has more directories at the end.
            yield $ Second snn
            advance _2
            passDirs _2 Second
            combineDirs
        (Just SureSep, Just SureSep) -> do
            -- Both sides have reached the end of subdirs.
            yield $ Both SureSep SureSep

            advanceBoth
            combineFiles
            combineDirs

        (Nothing, Nothing) ->
            -- All done with everything
            return ()

        -- We should only get here if the tree is invalid.
        _ -> error $ show (view firstNode st0) ++ " & " ++ show (view secondNode st0)

-- Called after just visiting the Enter node on a particular side.
-- The lens is the accessor for the desired side.  This side will be
-- advanced (and passed through) until this directory is done.  The
-- make argument constructs the ComboNode for the return.
passDirs
    :: Monad m
    => Lens' (CombState m) (OneState m)
    -> (SureNode -> ComboNode SureNode)
    -> ConduitT () (ComboNode SureNode) (CombStateT m) ()
passDirs lns make = do
    node <- lift $ view (lns . _2) <$> get
    advance lns
    case node of
        Just nn@(SureEnter _ _) -> do
            yield $ make nn
            passDirs lns make -- recursive call
            passDirs lns make -- tail call
        Just SureLeave -> do
            yield $ make SureLeave
            -- Done with this dir.
        Just nn -> do
            yield $ make nn
            passDirs lns make -- tail call
        Nothing ->
            error "Unexpected end of input on stream"

-- Walk through the list of files, yielding them in order as they are
-- seen by first and second.
combineFiles :: Monad m => ConduitT () (ComboNode SureNode) (CombStateT m) ()
combineFiles = do
    st0 <- lift get
    case (view firstNode st0, view secondNode st0) of
        (Just fnn@(SureNode fname _), Just snn@(SureNode sname _))
            | fname == sname -> do
                yield $ Both fnn snn
                advanceBoth
                combineFiles
            | fname < sname -> do
                -- First has file second doesn't
                yield $ First fnn
                advance _1
                combineFiles
            | otherwise -> do
                -- Second has file second doesn't
                yield $ Second fnn
                advance _2
                combineFiles
        (Just fnn@(SureNode _ _), Just SureLeave) -> do
            -- First has file second doesn't
            yield $ First fnn
            advance _1
            combineFiles
        (Just SureLeave, Just snn@(SureNode _ _)) -> do
            -- Second has file second doesn't
            yield $ First snn
            advance _2
            combineFiles
        (Just SureLeave, Just SureLeave) -> do
            -- End of file section.
            yield $ Both SureLeave SureLeave
            advanceBoth
        _ -> error "Unexpected state, corrupt surefile"

-- Advance both.  TODO: I don't know if a traversal can do this,
-- probably.
advanceBoth :: Monad m => ConduitT () b (CombStateT m) ()
advanceBoth = advance _1 >> advance _2

-- Advance the pair described by the given lens.
advance
    :: Monad m
    => Lens' (CombState m) (OneState m)
    -> ConduitT () b (CombStateT m) ()
advance lns = do
    (src, _) <- view lns <$> lift get
    newPair <- lift $ lift $ src $$++ await
    lift $ modify' $ set lns $ newPair

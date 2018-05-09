-- |Parallel stream map

{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.ParMap (
    parMapAccumM
) where

import Conduit
import Control.Concurrent hiding (yield)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (unless)

-- |Perform a monadic transformation on the elements of the stream,
-- while maintaining a Monoidal state that has a Monadic update
-- operation.
--
-- The intent here is to perform some operation where we track
-- progress and have a way of updating that progress.
--
-- Note that the async package is not generalized over IO, so we can't
-- be either.
parMapAccumM
    :: forall a b m s. (MonadIO m, Monoid s)
    => (a -> IO (s, b))
    -- ^Transformation to be run in parallel.  Returns the new stream
    -- element 'b' that will be yielded.  The 's' returned will be
    -- combined with the current state (using mappend) and the
    -- progress update function called.
    -> (s -> IO ())
    -- ^Update the progress meter with the new state.
    -> s
    -- ^The initial state.
    -> ConduitT a b m s
parMapAccumM op update s0 = do
    nCPU <- liftIO $ getNumCapabilities

    -- The queue of pending work.  Pending work is the content of this
    -- queue, plus possible one work item living in the 'st' register.
    work <- liftIO $ newTBQueueIO nCPU

    -- First action
    firstWork <- (liftIO . mapM (async . op)) =<< await

    let
        -- The main loop, tries processing all of the work.
        loop :: s -> Maybe (Async (s, b)) -> ConduitT a b m s
        loop st (Just workItem) = do
            -- There is still at least one outstanding work item.
            res <- liftIO $ atomically (pushWork work workItem `orElse` popResult work)
            case res of
                Pushed -> do
                    -- Successfully pushed, get another work item.
                    nextWork <- (liftIO . mapM (async . op)) =<< await
                    loop st nextWork
                Popped (sUpdate, b) -> do
                    -- Work has completed.
                    yield b
                    -- And update the progress meter.
                    let st' = st `mappend` sUpdate
                    liftIO $ update st'
                    loop st' (Just workItem)
                Done -> error "Unexpected state"
        loop st Nothing = do
            -- Now we are just waiting for everything to finishe.
            res <- liftIO $ atomically (popResult work `orElse` drained work)
            case res of
                Popped (sUpdate, b) -> do
                    -- TODO: This code is duplicated.
                    yield b
                    let st' = st `mappend` sUpdate
                    liftIO $ update st'
                    loop st' Nothing
                Done ->
                    -- Everything is done, return the final state
                    return st
                Pushed -> error "Unexpected state"

    loop s0 firstWork

data Action b
    = Pushed   -- We pushed at item
    | Popped b -- A work item finished, this is the result
    | Done     -- Everything is finished

-- Try pushing the work to the given queue.  Returns "Pushed" if that
-- is what it did.
pushWork :: TBQueue a -> a -> STM (Action b)
pushWork q item = do
    writeTBQueue q item
    return $ Pushed

-- Pop a result, from the queue, succeeding when the full result is
-- present.
popResult :: TBQueue (Async a) -> STM (Action a)
popResult q = do
    item <- readTBQueue q
    res <- waitSTM item
    return $ Popped res

-- Result in Done when the entire queue is empty.
drained :: TBQueue a -> STM (Action b)
drained q = do
    isEmpty <- isEmptyTBQueue q
    unless isEmpty $ retry
    return $ Done

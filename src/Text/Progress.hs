-- | A simple progress meter.

module Text.Progress (
    PMeter,
    withPMeter,
    newPMeter,
    pmPut,
    withPmHidden,
    pmFlush
) where

import Control.Concurrent.MVar
import Data.Time.Clock

-- A progress meter consists of some text that can be updated
-- periodically.  It can also be temporarily hidden (for example so
-- that other messages can be printed).
data PMData = PMData {
    -- |The current displayed meter.  If Nothing, then nothing has
    -- been printed, otherwise it is the meter text.  The text can
    -- contain newlines, which will be used to determine how far back
    -- to move the cursor to clear the meter.  It does not need a
    -- final newline, which will be added by the printing.
    current :: Maybe String,

    -- |The string we'd like to print if we update.  If Nothing, the
    -- current meter will continue to be displayed.
    desired :: Maybe String,

    -- |The next time we should update the meter.
    next :: UTCTime }
    deriving Show

type PMeter = MVar PMData

interval :: NominalDiffTime
interval = 0.250

-- |Perform an action with a progress meter, flushing when done.  If
-- an exception is raised, the current display will be left in place.
withPMeter :: (PMeter -> IO a) -> IO a
withPMeter action = do
    pmeter <- newPMeter
    result <- action pmeter
    pmFlush pmeter
    return result

-- |Construct a new progress meter.  It will initially be blank.
newPMeter :: IO PMeter
newPMeter = do
    now <- getCurrentTime
    newMVar $ PMData {
        current = Nothing,
        desired = Nothing,
        next = addUTCTime interval now }

pmPut :: PMeter -> String -> IO ()
pmPut pmeter msg = do
    modifyMVar pmeter $ \pm -> do
        now <- getCurrentTime
        if now `after` next pm
            then do
                clear $ current pm
                putStrLn $ msg
                return (PMData {
                    current = Just msg,
                    desired = Nothing,
                    next = addUTCTime interval now }, ())
            else return (pm { desired = Just msg }, ())

-- |With the message cleared, perform some IO (that presumably prints
-- something), and then restore any display.  The restore may update
-- with newer text, if one is available.
withPmHidden :: PMeter -> IO a -> IO a
withPmHidden pmeter action =
    modifyMVar pmeter $ \pm -> do
        clear $ current pm
        result <- action
        let msg = maybe (maybe "" id $ current pm) id $ desired pm
        putStrLn $ msg
        return (pm {
            current = Just msg,
            desired = Nothing }, result)

-- |Make sure the meter is updated with the latest message
pmFlush :: PMeter -> IO ()
pmFlush pmeter =
    modifyMVar pmeter $ \pm -> do
        case desired pm of
            Nothing -> return (pm, ())
            Just msg -> do
                clear $ current pm
                putStrLn $ msg
                return (pm { current = Just msg, desired = Nothing }, ())

clear :: Maybe String -> IO ()
clear =
    maybe (return ()) (\t -> do
        let count = 1 + length (filter (=='\n') t)
        putStr $ "\ESC[" ++ show count ++ "A\ESC[0J")

after :: UTCTime -> UTCTime -> Bool
after a b = diffUTCTime a b >= 0

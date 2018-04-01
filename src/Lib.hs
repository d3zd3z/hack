module Lib
    ( someFunc
    ) where

import Hack.Zfs
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, FormatTime, defaultTimeLocale)

someFunc :: IO ()
someFunc = do
   zs <- getZfs
   now <- getCurrentTime
   putStrLn $ show $ length $ zfsEntries zs
   putStrLn $ "snap: " ++ snapName "prefix" now

-- Using the given formattable time, and the given prefix, return a
-- snapshot name.
snapName :: FormatTime t => String -> t -> String
snapName prefix now = prefix ++ "-" ++ formatTime defaultTimeLocale "%0Y%m%d%H%M" now

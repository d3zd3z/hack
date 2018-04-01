module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Hack.Zfs

someFunc :: IO ()
someFunc = do
   zs <- getZfs
   putStrLn $ intercalate "\n" $ map show $ zfsEntries zs

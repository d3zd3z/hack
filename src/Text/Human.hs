-- |Human friendly text output

module Text.Human (
    humanizeBytes
) where

import Text.Printf (printf)

units :: [String]
units = ["B  ", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]

-- |Generate a human readable size description.
humanizeBytes :: Integral a => a -> String
humanizeBytes = loop units . fromIntegral
    where
        loop :: [String] -> Double -> String
        loop (x:xs) num
            | num <= 1024.0 = printf (fmt num) num x
            | otherwise     = loop xs (num / 1024.0)
        loop [] _ = " INF     "

        fmt :: Double -> String
        fmt num
            | num < 10.0  = "%6.3f%s"
            | num < 100.0 = "%6.2f%s"
            | otherwise   = "%6.1f%s"

-- Hash testing

module Sure.HashTest (
    testHash,
    testToFile
) where

import qualified Data.Binary as Binary
import qualified Crypto.Hash as H
import qualified Data.ByteArray as DBA
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import Data.Foldable (for_)
import Data.List (mapAccumL, sortOn)
import qualified Data.Set as Set
import Data.Set (Set)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import System.IO (withFile, IOMode(..), hPutStrLn)

-- To simulate hashing performance in parallel, we keep a list of the
-- sizes of files on the system.
testHash :: IO ()
testHash = do
    conn <- connectSqlite3 "sample.db"
    _ <- run conn "CREATE TABLE nodes (idx INTEGER PRIMARY KEY, hash BLOB)" []
    putStrLn $ "Loading sizes"
    sizes <- convertSizes
    putStrLn $ "Storing " ++ show (length sizes) ++ " nodes"
    for_ sizes $ \size -> do
        _ <- run conn "INSERT INTO nodes VALUES (?, ?)" [toSql size, toSql $ numHash size]
        return ()
    commit conn
    putStrLn "Loading nodes"
    disconnect conn
    testQuery

testQuery :: IO ()
testQuery = do
    conn <- connectSqlite3 "sample.db"
    stmt <- prepare conn $ "SELECT idx, hash FROM nodes ORDER by idx"
    _ <- execute stmt []
    let
        loop buf = do
            row <- fetchRow stmt
            case row of
                Nothing -> return buf
                Just [_, hash] ->
                    loop (B.head (fromSql hash) : buf)
                Just _ -> error "Invalid response"
    allNodes <- loop []
    putStrLn $ show (length allNodes) ++ " nodes found"
    disconnect conn

testToFile :: IO ()
testToFile = do
    putStrLn "Loading sizes"
    sizes <- convertSizes
    putStrLn $ "Writing to file"
    withFile "sample.txt" WriteMode $ \fd -> do
        for_ sizes $ \size -> do
            hPutStrLn fd $ show size ++ " " ++ (C8.unpack $ hexifyB (numHash size))

-- Compute the hash of a number.
numHash :: Int -> B.ByteString
numHash num =
    let raw = Binary.encode num in
    let hash = H.hashlazy raw :: H.Digest H.SHA1 in
    B.pack $ DBA.unpack hash

hexifyB :: B.ByteString -> B.ByteString
hexifyB =
    L.toStrict .
        B.toLazyByteString .
        mconcat .
        map B.word8HexFixed .
        B.unpack

-- Read the sizes in, ultimately giving us a list of integers indices
-- in the order they should be inserted into the database.
convertSizes :: IO [Int]
convertSizes = toFinals 4 <$> getSizes

-- Read in the sizes file, returning all of the sizes as integers
getSizes :: IO [Integer]
getSizes = (map read . lines) <$> readFile "sizes"

putSizes :: [Integer] -> IO ()
putSizes = writeFile "sizes-new" . unlines . map show

-- Round a set of numbers up to a convenient multiple (should make
-- things compress better)
roundUp :: Integer -> Integer -> Integer
roundUp base x = ((x + base - 1) `div` base) * base

-- *

-- Given a list of durations, pairs of the indices that they complete
-- in.
toFinals :: Int -> [Integer] -> [Int]
toFinals ncpu nums =
    let (_, tpairs) = mapAccumL getCpu (state0 ncpu) nums in
    let times = zipWith (\(t,_) n -> (t, n)) tpairs [1..] in
    map snd $ sortOn fst times

type State = Set (Integer, Int)

-- Construct the initial state for 'n' cpus.
state0 :: Int -> State
state0 n = Set.fromList [(0, n) | n <- [1..n]]

-- Perform a computation taking 'time' units of time, returning the
-- new state, and the time it finished and CPU it ran on.
getCpu :: State -> Integer -> (State, (Integer, Int))
getCpu st1 ticks =
    let ((t1, cpu), st2) = Set.deleteFindMin st1 in
    let new = (ticks + t1, cpu) in
    let st3 = Set.insert new st2 in
    (st3, (ticks + t1, cpu))

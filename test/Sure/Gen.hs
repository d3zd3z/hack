-- |Sure tree generation.

{-# LANGUAGE OverloadedStrings #-}

module Sure.Gen (
    genTree,
    t1
) where

import Conduit
import Control.Monad (forM_, replicateM)
import qualified Data.ByteString.Char8 as B
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Random
-- import Pipes
-- import qualified Pipes.Prelude as PP

import Sure.Types

type RVarIO = RVarT IO

t1 :: IO ()
t1 = runConduit $ genTree 5 5 4 .| printC

-- Note that the rvar is run multiple times.  If the random generation
-- is slow, then this is probably not a good way to do this.  It is
-- likely adequate for testing, though.

-- |Produce a sure tree, with roughly 'files' files in each directory,
-- about 'dirs' subdirs, and a depth of about 'depth'.
genTree :: Int -> Int -> Int -> ConduitT () SureNode IO ()
genTree ofiles odirs odepth =
    transPipe (\m -> runRVarT m StdRandom) pipe
    where
    pipe :: ConduitT () SureNode RVarIO ()
    pipe = genTree' "__root__" ofiles odirs odepth 0
    genTree' name files dirs depth curDep
        | curDep >= depth = return ()
        | otherwise = do
            myAtts <- lift $ randAtts "dir"
            yield $ SureEnter name myAtts
            dirs' <- lift $ normalize dirs
            dirNames <- lift $ sort <$> replicateM dirs' (normalize 16 >>= randName)
            forM_ dirNames $ \dirName -> do
                depth' <- lift $ (min depth) <$> normalize depth
                genTree' dirName files dirs depth' (curDep + 1)
            yield $ SureSep
            files' <- lift $ normalize files
            fileNames <- lift $ sort <$> replicateM files' (normalize 16 >>= randName)
            mapM_ makeFile fileNames
            yield $ SureLeave

makeFile :: B.ByteString -> ConduitT () SureNode RVarIO ()
makeFile name = do
    myAtts <- lift $ randAtts "file"
    yield $ SureNode name myAtts

normalize :: Int -> RVarIO Int
normalize num = do
    let fnum = fromIntegral num :: Double
    a <- normalT fnum (fnum * 0.25)
    return $ (floor a) `max` 1

randAtts :: B.ByteString -> RVarIO AttMap
randAtts kind = do
    v <- normalT 1000 100 :: RVarIO Double
    let vint = floor v :: Int
    let bvalue = B.pack $ show vint
    return $ Map.fromList [("kind", kind), ("key", bvalue)]

randName :: Int -> RVarIO B.ByteString
randName desiredLen = do
    len <- normalize desiredLen
    chars <- replicateM len $ uniformT 'a' 'z'
    return $ B.pack chars

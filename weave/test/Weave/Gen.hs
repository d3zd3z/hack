-- |Test generation for weave tests.

module Weave.Gen (
   simEdits,
) where

-- Explicitly ask for the Lazy version, as we need it.
import Control.Monad.State.Lazy
import System.Random

-- Simulate edit operations on a list of integers of length 'len'.
simEdits :: Int -> [[Int]]
simEdits len = evalState (genPermutations [1..len]) st0
   where st0 = newGen len [len+1 ..]

type Gen a = State (GenState a)

data GenState a = GenState {
   targetLen :: !Int,
   newItems :: [a],
   stateRng :: !StdGen }
   deriving (Show)

newGen :: Int -> [a] -> GenState a
newGen tLen new = GenState {
   targetLen = tLen,
   newItems = new,
   stateRng = mkSane 1 }

-- A state for debugging.
-- st1 = newGen 50 [100..]

-- Return a unbounded list of permutations.  Note that if 'newItems'
-- is not unbounded, this will likely result in non-termination.
genPermutations :: Eq a => [a] -> Gen a [[a]]
genPermutations xs = do
   more <- genAction xs
   if xs == more then genPermutations xs
   else do
      xs' <- genPermutations more
      return $ xs : xs'

-- Randomly generate an insert, remove, or move
genAction :: [a] -> Gen a [a]
genAction xs = do
   op <- getR (0, 2)
   case op of
      0 -> genMove xs
      1 -> genInsert xs
      _ -> genRemove xs

-- Get a random value in (a, b) from the states generator.
getR :: (Int, Int) -> Gen a Int
getR range = do
   state $ \st ->
      let rng = stateRng st in
      let (result, rng') = randomR range rng in
      (result, st { stateRng = rng' })

-- Given a length, return a non-decreasing subrange of that length.
-- The (a, b) of the results will be between 0 and len, inclusive, and
-- a < b.
getRange :: Int -> Gen a (Int, Int)
getRange len = do
   a' <- getR (0, len)
   b' <- getR (0, len)
   return $ if a' < b' then (a', b') else (b', a')

-- Perform a random movement of the list.
genMove :: [a] -> Gen a [a]
genMove xs = do
   let len = length xs
   (a, b) <- getRange len
   c <- getR (0, len - (b - a))
   let (sub, chunk) = remove a b xs
   return $ insert c chunk sub

-- Perform a random removal from the list.
genRemove :: [a] -> Gen a [a]
genRemove xs = do
   let len = length xs
   (a, b) <- getRange len
   let (left, _) = remove a b xs
   return left

-- Generate an insertion.  This targets a number that strives to keep
-- the list with targetLen items in it.  If we are over the target,
-- just return the same list, and deduplication will remove it.
genInsert :: [a] -> Gen a [a]
genInsert xs = do
   let len = length xs
   tlen <- gets targetLen
   if len >= 2 * tlen then return xs
   else do
      count <- getR (1, 2 * (tlen - len))
      pos <- getR (0, len)
      items <- state $ \st -> do
         let ys = newItems st
         let (items, ys') = splitAt count ys
         (items, st { newItems = ys' })
      return $ insert pos items xs

-- The standard generator is pretty terrible at the beginning, discard
-- a bunch of random "values" from the generator to make a more
-- meaningful.
mkSane :: Int -> StdGen
mkSane = mkSane' (50 :: Int) . mkStdGen
   where
      mkSane' 0 g = g
      mkSane' n g =
         let (_, g') = next g in mkSane' (n-1) g'

-- |Remove the elements `[a-b)` from the list.  Returns a pair of the
-- new list, and the removed elements.
remove :: Int -> Int -> [a] -> ([a], [a])
remove a b xs = (left ++ right, zs)
   where
      (left, ys) = splitAt a xs
      (zs, right) = splitAt (b - a) ys

-- |Insert list `ys` into the `xs` list at `pos'
insert :: Int -> [a] -> [a] -> [a]
insert a ys xs = left ++ ys ++ right
   where (left, right) = splitAt a xs

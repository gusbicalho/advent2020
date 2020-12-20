{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad.ST.Strict (ST)
import Control.Monad.ST.Strict qualified as ST
import Data.Foldable qualified as Foldable
import Data.Functor (($>))
import Data.HashTable.Class qualified as HashTable
import Data.HashTable.ST.Linear (HashTable)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.STRef.Strict (STRef)
import Data.STRef.Strict qualified as STRef
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Formatting qualified
import Formatting.Clock qualified
import System.Clock qualified as Clock

main :: IO ()
main = do
  putStrLn "Part 1"
  part1 "solveWithMutableHashtable" solveWithMutableHashtable
  part1 "solveWithIterate" solveWithIterate
  putStrLn "Part 2"
  part2 "solveWithMutableHashtable" solveWithMutableHashtable
  part2 "solveWithIterate" solveWithIterate
 where
  part1 label solver =
    timing label $ do
      Foldable.for_ examples $ \example ->
        putStrLn . ("example " <>) . show . (Foldable.toList &&& solve1 solver) $ example
      putStrLn . ("for real: " <>) . show . solve1 solver $ input
  part2 label solver =
    timing label $ do
      putStrLn . ("for real: " <>) . show . solve2 solver $ input
  timing :: String -> IO a -> IO a
  timing label action = do
    putStrLn $ label <> " - begin"
    start <- Clock.getTime Clock.Monotonic
    !result <- action
    end <- Clock.getTime Clock.Monotonic
    putStrLn $
      label
        <> " - Computation time: "
        <> Formatting.formatToString Formatting.Clock.timeSpecs start end
    pure result

solve1 :: (Int -> Seq Word -> Word) -> Seq Word -> Word
solve1 solver = solver (2020 - 1)

solve2 :: (Int -> Seq Word -> Word) -> Seq Word -> Word
solve2 solver = solver (30000000 - 1)

-- >>> solveWithMutableHashtable (2020 - 1) . Seq.fromList $ [0,3,6]
-- 436
-- Good performance with HashTable.ST.Linear, other options didn't perform well.
-- No idea why.
-- This seems to be ~25% faster than the immutable-Map-based solution below.
solveWithMutableHashtable :: Int -> Seq Word -> Word
solveWithMutableHashtable ix ws = ST.runST $ do
  let ((prefixIndex, prefixMemory), latest) = loadPrefix ws
  memory_' <- HashTable.new
  Foldable.for_ (Map.toAscList prefixMemory) $
    uncurry (HashTable.insert memory_')
  index_' <- STRef.newSTRef prefixIndex
  doTimes (ix + 1 - Seq.length ws) (stepOnST index_' memory_') latest
 where
  doTimes :: Monad m => Int -> (a -> m a) -> a -> m a
  doTimes !n f zero
    | n <= 0 = pure zero
    | otherwise = f zero >>= doTimes (pred n) f
  stepOnST :: STRef s Word -> HashTable s Word Word -> Word -> ST s Word
  stepOnST index_' memory_' latest = do
    index <- STRef.readSTRef index_'
    let next = do
          STRef.modifySTRef' index_' succ
          HashTable.insert memory_' latest index
    HashTable.lookup memory_' latest >>= \case
      Nothing -> next $> 0
      Just previousIndex -> next $> index - previousIndex

-- >>> solveWithIterate (2020 - 1) . Seq.fromList $ [0,3,6]
-- 436
-- At first i implemented this with Prelude.iterate, but that caused a space leak!
-- It was very slow and sometimes it even died because OOM.
-- I think it's because !! only force the spine of the list, so the ix-th item
-- would have a huge tower of thunks, all holding on to the previous states.
-- List.iterate' forces each item on the list, so when we get to the ix-th item,
-- it's just a plain Word.
solveWithIterate :: Int -> Seq Word -> Word
solveWithIterate ix ws = snd . (!! (ix + 1 - Seq.length ws)) . List.iterate' step $ loadPrefix ws

step :: ((Word, Map Word Word), Word) -> ((Word, Map Word Word), Word)
step ((!index, !memory), !latest) =
  let nextState = (succ index, Map.insert latest index memory)
   in case Map.lookup latest memory of
        Nothing -> (nextState, 0)
        Just previousIndex -> (nextState, index - previousIndex)

loadPrefix :: Seq Word -> ((Word, Map Word Word), Word)
loadPrefix = \case
  Seq.Empty -> error "empty prefix"
  x :<| xs -> goPrefix ((1, Map.empty), x) xs
 where
  goPrefix state Seq.Empty = state
  goPrefix ((!index, !memory), !latest) (x :<| xs) =
    goPrefix ((succ index, Map.insert latest index memory), x) xs

examples :: [Seq Word]
examples =
  [ Seq.fromList [0, 3, 6]
  , Seq.fromList [1, 3, 2]
  , Seq.fromList [2, 1, 3]
  , Seq.fromList [1, 2, 3]
  , Seq.fromList [2, 3, 1]
  , Seq.fromList [3, 2, 1]
  , Seq.fromList [3, 1, 2]
  ]

input :: Seq Word
input = Seq.fromList [20, 9, 11, 0, 1, 2]

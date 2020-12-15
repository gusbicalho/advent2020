{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Advent2020.Day10 (day10_01, day10_02) where

import Advent2020.Input qualified
import Control.Monad.ST (ST)
import Control.Monad.ST qualified as ST
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as ReaderT
import Data.Array.ST qualified as Array
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Traversable (for)
import Data.Word (Word64)
import Text.Read qualified as Read

day10_01 :: IO Word
day10_01 = solve01 <$> input

-- solve01 :: Seq Word -> Word
solve01 :: Seq Word -> Word
solve01 ratings = uncurry (*) ratingDiffs
 where
  ratingDiffs =
    ratings
      & Seq.sort
      & Foldable.foldl' recordDiff (initialDiffs, 0)
      & fst
      & addDiff3
  initialDiffs = (0,0)
  addDiff1 (diffs1, diffs3) = (succ diffs1, diffs3)
  addDiff3 (diffs1, diffs3) = (diffs1, succ diffs3)
  recordDiff (diffs, prevRating) rating =
    case rating - prevRating of
      1 -> (addDiff1 diffs, rating)
      3 -> (addDiff3 diffs, rating)
      _ -> (diffs, rating)

-- >>> solve01 $ example
-- 220

-- >>> day10_01
-- 2263

day10_02 :: IO Word64
day10_02 = solve02 <$> input

solve02 :: Seq Word -> Word64
solve02 ratings =
  runMemo (Seq.length sortedRatings) $
    go 0 0 (Foldable.toList sortedRatings)
 where
  sortedRatings = ratings & Seq.sort & addBuiltIn
  addBuiltIn Seq.Empty = Seq.fromList [3]
  addBuiltIn s@(_ Seq.:|> lastAdapter) = s Seq.|> lastAdapter + 3
  go :: Int -> Word -> [Word] -> Memo Word64 Word64
  go _index _ [] = pure 1
  go index outputJolts adapters = withMemo index $ do
    let tails = fittingTails index outputJolts adapters
    acceptableTails <- for tails $ \(tailIndex, jolts NonEmpty.:| moreAdapters) ->
      go tailIndex jolts moreAdapters
    pure $! sum acceptableTails
  fittingTails :: Int -> Word -> [Word] -> [(Int, NonEmpty Word)]
  fittingTails index outputJolts =
    zip [index + 1 ..]
      . takeWhile (adapterFits outputJolts . NonEmpty.head)
      . Maybe.mapMaybe NonEmpty.nonEmpty
      . List.tails

-- >>> solve02 $ small
-- 8

-- >>> solve02 $ example
-- 19208

-- >>> day10_02
-- 396857386627072

-- Poor man's monad transformer
newtype Memo memo a = Memo { unMemo :: forall s. ReaderT (Array.STArray s Int (Maybe memo)) (ST s) a }
  deriving stock (Functor)

instance Applicative (Memo memo) where
  pure a = Memo (pure a)
  Memo f <*> Memo a = Memo $ f <*> a
instance Monad (Memo memo) where
  (Memo ma) >>= f = Memo $ ma >>= unMemo . f

runMemo :: Int -> Memo a a -> a
runMemo memoSize (Memo action) = ST.runST $ do
  memo <- Array.newArray (0, memoSize) Nothing
  ReaderT.runReaderT action memo

withMemo :: Int -> Memo a a -> Memo a a
withMemo k (Memo action) = Memo $ do
  memoRef <- ReaderT.ask
  Trans.lift (Array.readArray memoRef k) >>= \case
    Just v -> pure v
    Nothing -> do
      v <- action
      Trans.lift $ Array.writeArray memoRef k (Just v)
      pure v

adapterFits :: Word -> Word -> Bool
adapterFits jolts rating = jolts + 3 >= rating

small :: Seq Word
small =
  Seq.fromList
    [ 16
    , 10
    , 15
    , 5
    , 1
    , 11
    , 7
    , 19
    , 6
    , 12
    , 4
    ]

example :: Seq Word
example =
  Seq.fromList
    [ 28
    , 33
    , 18
    , 42
    , 31
    , 14
    , 46
    , 20
    , 48
    , 47
    , 24
    , 23
    , 49
    , 45
    , 19
    , 38
    , 39
    , 11
    , 1
    , 32
    , 25
    , 35
    , 8
    , 17
    , 7
    , 9
    , 4
    , 2
    , 34
    , 10
    , 3
    ]

input :: IO (Seq Word)
input = Advent2020.Input.loadInput (Read.readMaybe @Word) "resources/day10/input"

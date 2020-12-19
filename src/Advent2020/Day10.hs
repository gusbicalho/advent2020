{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
<<<<<<< HEAD
=======
{-# LANGUAGE DeriveFunctor #-}
>>>>>>> 6b2b1f544eeb94241d19fe9e160da7e4a2904d4d
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
<<<<<<< HEAD
=======
{-# LANGUAGE RankNTypes #-}
>>>>>>> 6b2b1f544eeb94241d19fe9e160da7e4a2904d4d
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Advent2020.Day10 (day10_01, day10_02) where

import Advent2020.Input qualified
import Control.Monad.ST (ST)
import Control.Monad.ST qualified as ST
<<<<<<< HEAD
import Data.Coerce (coerce)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Semigroup qualified as Semigroup
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Text.Read qualified as Read
import Data.Traversable (for)
import Control.Monad.Trans.State.Strict (State, StateT)
import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Control.Monad.Trans.Class as Trans
=======
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as ReaderT
import Data.Array.ST qualified as Array
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:|>)), (|>))
import Data.Sequence qualified as Seq
import Data.Traversable (for)
import Data.Word (Word64)
import Text.Read qualified as Read
>>>>>>> 6b2b1f544eeb94241d19fe9e160da7e4a2904d4d

day10_01 :: IO Word
day10_01 = solve01 <$> input

-- solve01 :: Seq Word -> Word
solve01 :: Seq Word -> Word
<<<<<<< HEAD
solve01 ratings = howMany 1 ratingDiffs * howMany 3 ratingDiffs
=======
solve01 ratings = uncurry (*) ratingDiffs
>>>>>>> 6b2b1f544eeb94241d19fe9e160da7e4a2904d4d
 where
  ratingDiffs =
    ratings
      & Seq.sort
<<<<<<< HEAD
      & Foldable.foldl' recordDiff (emptyBag :: Bag Word, 0)
      & fst
      & addToBag 3
  recordDiff (diffs, prevRating) rating = (addToBag (rating - prevRating) diffs, rating)
=======
      & Foldable.foldl' recordDiff (initialDiffs, 0)
      & fst
      & addDiff3
  initialDiffs = (0, 0)
  addDiff1 (diffs1, diffs3) = (succ diffs1, diffs3)
  addDiff3 (diffs1, diffs3) = (diffs1, succ diffs3)
  recordDiff (diffs, prevRating) rating =
    case rating - prevRating of
      1 -> (addDiff1 diffs, rating)
      3 -> (addDiff3 diffs, rating)
      _ -> (diffs, rating)
>>>>>>> 6b2b1f544eeb94241d19fe9e160da7e4a2904d4d

-- >>> solve01 $ example
-- 220

-- >>> day10_01
-- 2263

<<<<<<< HEAD
day10_02 :: IO Int
day10_02 = solve02 <$> input

solve02 :: Seq Word -> Int
solve02 ratings =
  length $
    flip StateT.evalState Map.empty $
      ratings
        & Seq.sort
        & addBuiltIn
        & Foldable.toList
        & go 0 0
 where
  addBuiltIn Seq.Empty = Seq.fromList [3]
  addBuiltIn s@(_ Seq.:|> lastAdapter) = s Seq.|> lastAdapter + 3
  go :: Int -> Word -> [Word] -> State (Map Int [[Word]]) [[Word]]
  go _index outputJolts [] = pure [[outputJolts]]
  go index outputJolts adapters = withMemo index $ do
    let tails = fittingTails index outputJolts adapters
    acceptableTails <- for tails $ \(tailIndex, jolts NonEmpty.:| moreAdapters) ->
      go tailIndex jolts moreAdapters
    pure . fmap (outputJolts :) . concat $ acceptableTails
=======
day10_02 :: IO Word64
day10_02 = solve02 <$> input

solve02 :: Seq Word -> Word64
solve02 ratings =
  runMemo (Seq.length sortedRatings) $
    go 0 0 (Foldable.toList sortedRatings)
 where
  sortedRatings = ratings & Seq.sort & addBuiltIn
  addBuiltIn Seq.Empty = Seq.fromList [3]
  addBuiltIn s@(_ :|> lastAdapter) = s |> lastAdapter + 3
  go :: Int -> Word -> [Word] -> Memo Word64 Word64
  go _index _ [] = pure 1
  go index outputJolts adapters = withMemo index $ do
    let tails = fittingTails index outputJolts adapters
    acceptableTails <- for tails $ \(tailIndex, jolts :| moreAdapters) ->
      go tailIndex jolts moreAdapters
    pure $! sum acceptableTails
>>>>>>> 6b2b1f544eeb94241d19fe9e160da7e4a2904d4d
  fittingTails :: Int -> Word -> [Word] -> [(Int, NonEmpty Word)]
  fittingTails index outputJolts =
    zip [index + 1 ..]
      . takeWhile (adapterFits outputJolts . NonEmpty.head)
      . Maybe.mapMaybe NonEmpty.nonEmpty
      . List.tails

<<<<<<< HEAD
withMemo :: (Monad m, Ord k) => k -> StateT (Map k v) m v -> StateT (Map k v) m v
withMemo k action = StateT.gets (Map.lookup k) >>= \case
  Just v -> pure v
  Nothing -> do
    v <- action
    StateT.modify' $ Map.insert k v
    pure v

=======
>>>>>>> 6b2b1f544eeb94241d19fe9e160da7e4a2904d4d
-- >>> solve02 $ small
-- 8

-- >>> solve02 $ example
-- 19208

-- >>> day10_02
<<<<<<< HEAD
-- ProgressCancelledException
=======
-- 396857386627072

-- Poor man's monad transformer
newtype Memo memo a = Memo {unMemo :: forall s. ReaderT (Array.STArray s Int (Maybe memo)) (ST s) a}
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
>>>>>>> 6b2b1f544eeb94241d19fe9e160da7e4a2904d4d

adapterFits :: Word -> Word -> Bool
adapterFits jolts rating = jolts + 3 >= rating

<<<<<<< HEAD
newtype Bag k = Bag {unBag :: Map k Word}

emptyBag :: Bag k
emptyBag = Bag Map.empty

addToBag :: Ord k => k -> Bag k -> Bag k
addToBag adapter (Bag m) = Bag $ Map.insertWith (+) adapter 1 m

-- takeFromBag :: Ord k => k -> Bag k -> Bag k
-- takeFromBag adapter (Bag m) = Bag $ Map.update remove adapter m
--  where
--   remove x
--     | x <= 1 = Nothing
--     | otherwise = Just (pred x)

howMany :: Ord k => k -> Bag k -> Word
howMany k (Bag m) = Maybe.fromMaybe 0 $ Map.lookup k m

instance Foldable Bag where
  foldMap toMonoid (Bag m) =
    m & Map.toAscList & foldMap (\(k, n) -> Semigroup.stimesMonoid n (toMonoid k))

=======
>>>>>>> 6b2b1f544eeb94241d19fe9e160da7e4a2904d4d
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

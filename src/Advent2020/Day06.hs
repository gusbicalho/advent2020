{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Advent2020.Day06 (day06_01, day06_02) where

import Advent2020.Input qualified
import Data.Foldable (Foldable (foldl'))
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup qualified as Semigroup
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Natural (Natural)

day06_01 :: IO Natural
day06_01 = solve01 <$> input

solve01 :: Seq String -> Natural
solve01 =
  sum
    . fmap (fromIntegral @Int @Natural . Set.size . Semigroup.sconcat)
    . toGroups
    . Foldable.toList

-- >>> day06_01
-- 6590

day06_02 :: IO Natural
day06_02 = solve02 <$> input

solve02 :: Seq String -> Natural
solve02 =
  sum
    . fmap (fromIntegral @Int @Natural . Set.size . intersectAll)
    . toGroups
    . Foldable.toList
 where
  intersectAll :: Ord a => NonEmpty (Set a) -> Set a
  intersectAll (x NonEmpty.:| xs) = foldl' Set.intersection x xs

-- >>> day06_02
-- 3288

type Group = NonEmpty (Set Char)

toGroups :: [String] -> [Group]
toGroups = fmap (fmap Set.fromList) . breakAll (== "")
 where
  breakAll :: (a -> Bool) -> [a] -> [NonEmpty a]
  breakAll p items = case break p (dropWhile p items) of
    ([], _) -> []
    (x : xs, []) -> [x NonEmpty.:| xs]
    (x : xs, moreItems) -> (x NonEmpty.:| xs) : breakAll p moreItems

input :: IO (Seq String)
input = Advent2020.Input.inputLines "resources/day06/input"

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Advent2020.Day09 (day09_01, day09_02) where

import Advent2020.Input qualified
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.TypeLits (KnownNat, Nat, natVal)
import Text.Read qualified as Read

day09_01 :: IO (Maybe Word)
day09_01 = solve01 @25 <$> input

solve01 :: forall (n :: Nat). KnownNat n => Seq Word -> Maybe Word
solve01 = findInvalidItem @n

day09_02 :: IO (Maybe Word)
day09_02 = solve02 @25 <$> input

solve02 :: forall (n :: Nat). KnownNat n => Seq Word -> Maybe Word
solve02 xs = do
  target <- findInvalidItem @n xs
  prefix <-
    Maybe.listToMaybe
      . Maybe.mapMaybe (prefixMatching target)
      . List.tails
      . Foldable.toList
      $ xs
  pure $ Foldable.minimum prefix + maximum prefix
 where
  prefixMatching :: Word -> [Word] -> Maybe (NonEmpty Word)
  prefixMatching _ [] = Nothing
  prefixMatching target (y : ys) = do
    let prefixSums = takeWhile (<= target) $ scanl (+) y ys
    finalSum <- maybeLast prefixSums
    if finalSum == target
      then NonEmpty.nonEmpty . take (length prefixSums) $ y : ys
      else Nothing

-- >>> solve02 @5 $ example
-- Just 62

-- >>> day09_02
-- Just 76096372

data XmasFrame (n :: Nat) = XmasFrame (Preamble n) [Word]

findInvalidItem :: forall (n :: Nat). KnownNat n => Seq Word -> Maybe Word
findInvalidItem = Maybe.listToMaybe . Maybe.mapMaybe invalidNextItem . xmasFrames @n
 where
  invalidNextItem :: XmasFrame n -> Maybe Word
  invalidNextItem frame = do
    item <- nextItem frame
    case validate item frame of
      Nothing -> Just item
      Just _ -> Nothing

validate :: Word -> XmasFrame n -> Maybe (Word, Word)
validate x (XmasFrame p _) = preambleMatches x p

nextItem :: XmasFrame n -> Maybe Word
nextItem (XmasFrame _ []) = Nothing
nextItem (XmasFrame _ (x : _)) = Just x

xmasFrames :: forall (n :: Nat). KnownNat n => Seq Word -> [XmasFrame n]
xmasFrames = List.unfoldr (>>= \frame -> Just (frame, nextFrame frame)) . xmasFrame @n

xmasFrame :: forall (n :: Nat). KnownNat n => Seq Word -> Maybe (XmasFrame n)
xmasFrame xs = preamble @n xs <&> \(p, moreXs) -> XmasFrame p (Foldable.toList moreXs)

nextFrame :: XmasFrame n -> Maybe (XmasFrame n)
nextFrame (XmasFrame _ []) = Nothing
nextFrame (XmasFrame pre (x : xs)) = Just $ XmasFrame (addToPreamble x pre) xs

data Preamble (n :: Nat) = Preamble (Seq Word) (Set Word)

preamble :: forall (n :: Nat). KnownNat n => Seq Word -> Maybe (Preamble n, Seq Word)
preamble xs = case Seq.splitAt preambleSize xs of
  (front, back)
    | length front < preambleSize -> Nothing
    | otherwise -> Just (Preamble @n front (Set.fromList . Foldable.toList $ front), back)
 where
  preambleSize :: Int
  preambleSize = fromIntegral $ natVal @n Proxy

addToPreamble :: Word -> Preamble n -> Preamble n
addToPreamble x p@(Preamble queue xSet) = case queue of
  Seq.Empty -> p -- 0-sized preamble
  dropped Seq.:<| queue' -> Preamble (queue' Seq.|> x) (xSet & Set.delete dropped & Set.insert x)

preambleMatches :: Word -> Preamble n -> Maybe (Word, Word)
preambleMatches item (Preamble _ xSet) =
  Foldable.find accepts low <&> \y -> (y, item - y)
 where
  half = item `div` 2
  (low, high) = fullSplit half xSet
  accepts y =
    let d = item - y
     in d /= y && (d `elem` high)

fullSplit :: Ord a => a -> Set a -> (Set a, Set a)
fullSplit i set = case Set.splitMember i set of
  (low, False, high) -> (low, high)
  (low, True, high) -> (Set.insert i low, high)

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast [a] = Just a
maybeLast (_ : xs) = maybeLast xs

example :: Seq Word
example =
  Seq.fromList
    [ 35
    , 20
    , 15
    , 25
    , 47
    , 40
    , 62
    , 55
    , 65
    , 95
    , 102
    , 117
    , 150
    , 182
    , 127
    , 219
    , 299
    , 277
    , 309
    , 576
    ]

input :: IO (Seq Word)
input = Advent2020.Input.loadInput (Read.readMaybe @Word) "resources/day09/input"

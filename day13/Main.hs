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

import Advent2020.Input qualified
import Control.Arrow (Arrow ((&&&)))
import Control.Exception (throwIO)
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Text.Read qualified as Read

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show $ solve1 example
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show $ solve2 example
  putStrLn . ("for real: " <>) . show . solve2 =<< input

-- >>> solve1 example
-- Just 295

solve1 :: BusNotes -> Maybe Integer
solve1 BusNotes{minDepartureTime, maybeBusIds} =
  fmap (uncurry (*)) . Maybe.listToMaybe . List.sortOn snd . fmap (id &&& waitTime minDepartureTime) $ busIds
 where
  busIds :: [BusId]
  busIds = Maybe.catMaybes . Foldable.toList $ maybeBusIds

-- >>> solve2 example
-- 1068781

solve2 :: BusNotes -> Integer
solve2 = chineseTheorem . offsetList . maybeBusIds

type Divisor = Integer
type Remainder = Integer
type Congruence = (Divisor, Remainder) -- x `mod` divisor = remainder

-- Some of this was based on https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Using_the_existence_construction
-- The rest was trial and error
chineseTheorem :: [Congruence] -> Integer
chineseTheorem (congA : congB : moreCongs) = go congA congB moreCongs
 where
  go (!div1, !rem1) (!div2, !rem2) more =
    let (_, (coefA, coefB)) = extendedGCD div1 div2
        delta = rem2 * coefA * div1 + rem1 * coefB * div2
        productOfDivs = div1 * div2
     in case more of
          [] -> modAbs productOfDivs delta
          (next : more') -> go (productOfDivs, delta) next more'
chineseTheorem _ = 0

extendedGCD :: Integral t => t -> t -> (t, (t, t))
extendedGCD a b = go a b 1 0 0 1
 where
  go oldR 0 oldS _ oldT _ = (oldR, (oldS, oldT))
  go oldR r oldS s oldT t =
    let q = oldR `div` r
     in go r (oldR - q * r) s (oldS - q * s) t (oldT - q * t)

offsetList :: Seq (Maybe BusId) -> [(BusId, Integer)]
offsetList =
  Maybe.catMaybes
    . Foldable.toList
    . Seq.mapWithIndex (\i mBusId -> mBusId <&> \busId -> (busId, modAbs busId (fromIntegral (- i))))

modAbs :: Integer -> Integer -> Integer
modAbs modulus i = (head . dropWhile (< 0) . iterate (modulus +) $ i) `mod` modulus

type BusId = Integer
data BusNotes = BusNotes
  { minDepartureTime :: Integer
  , maybeBusIds :: Seq (Maybe BusId)
  }
  deriving stock (Eq, Ord, Show)

waitTime :: Integer -> BusId -> Integer
waitTime minDepartureTime busId = case minDepartureTime `mod` busId of
  0 -> 0
  x -> busId - x

example :: BusNotes
example =
  Maybe.fromJust
    . parseBusNotes
    . Seq.fromList
    $ [ "939"
      , "7,13,x,x,59,x,31,19"
      ]

input :: IO BusNotes
input = do
  inputLines <- Advent2020.Input.loadInput Just "day13/input"
  case parseBusNotes inputLines of
    Just notes -> pure notes
    Nothing -> throwIO . userError $ "Bad input"

parseBusNotes :: Seq String -> Maybe BusNotes
parseBusNotes (minDepartureTimeEstimateLine :<| busIdsLine :<| _) = do
  minDepartureTime <- Read.readMaybe @Integer minDepartureTimeEstimateLine
  let busIdStrs = splitWhen (== ',') busIdsLine
      maybeBusIds = Seq.fromList . fmap (Read.readMaybe @BusId) $ busIdStrs
  pure $ BusNotes{minDepartureTime, maybeBusIds}
 where
  splitWhen :: (a -> Bool) -> [a] -> [[a]]
  splitWhen p xs = case break p (dropWhile p xs) of
    ([], _) -> []
    (ys, moreXs) -> ys : splitWhen p moreXs
parseBusNotes _ = Nothing

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

module Advent2020.Day11 (day11_01, day11_02) where

import Advent2020.Input qualified
import Control.Applicative
    ( Applicative(liftA2), ZipList(ZipList, getZipList) )
import Data.Foldable qualified as Foldable
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

day11_01 :: IO Int
day11_01 = solve01 <$> input

solve01 :: SeatMap -> Int
solve01 = countSpots (== Occupied) . stepUntilStable adjacentSeatsRule
 where
  countSpots predicate = length . filter predicate . toSpotList

-- >>> solve01 $ example
-- 37

-- >>> day11_01
-- 2299

day11_02 :: IO Int
day11_02 = solve02 <$> input

solve02 :: SeatMap -> Int
solve02 = countSpots (== Occupied) . stepUntilStable lineOfSightRule
 where
  countSpots predicate = length . filter predicate . toSpotList

-- >>> solve02 $ example
-- 26

-- >>> day11_02
-- 2047

data Spot = Floor | Free | Occupied
  deriving stock (Eq, Ord, Show)
newtype SeatMap = SeatMap (Seq (Seq Spot))
  deriving stock (Eq)

type Rule = SeatRegion -> Spot
data SeatRegion = SeatRegion
  { focus :: (Int, Int)
  , spot :: Spot
  , seatMap :: SeatMap
  }

lineOfSightRule :: Rule
lineOfSightRule SeatRegion{spot, focus, seatMap} = case spot of
  Free | occupied == 0 -> Occupied
  Occupied | occupied >= 5 -> Free
  _ -> spot
 where
  occupied = length . filter (== Just Occupied) . fmap nextSeat . uncurry linesOfSight focus $ seatMap
  nextSeat [] = Nothing
  nextSeat (Floor : seats) = nextSeat seats
  nextSeat (seat : _) = Just seat

linesOfSight :: Int -> Int -> SeatMap -> [[Spot]]
linesOfSight x y seatMap =
  fmap toSpots directionsOfSight
 where
  toSpots :: [(Int, Int)] -> [Spot]
  toSpots coords =
    Maybe.catMaybes
      . takeWhile Maybe.isJust
      . fmap (\(dx, dy) -> spotAt (x + dx) (y + dy) seatMap)
      $ coords

directionsOfSight :: [[(Int, Int)]]
directionsOfSight =
  [ direction stay north
  , direction east north
  , direction east stay
  , direction east south
  , direction stay south
  , direction west south
  , direction west stay
  , direction west north
  ]
 where
  direction dxs dys = getZipList $ liftA2 (,) dxs dys
  north = ZipList [-1, -2 ..]
  east = north
  south = ZipList [1, 2 ..]
  west = south
  stay = ZipList [0, 0 ..]

adjacentSeatsRule :: Rule
adjacentSeatsRule SeatRegion{spot, focus, seatMap} = case spot of
  Free | occupied == 0 -> Occupied
  Occupied | occupied >= 4 -> Free
  _ -> spot
 where
  occupied = length . filter (== Occupied) . uncurry adjacentSpots focus $ seatMap

adjacentSpots :: Int -> Int -> SeatMap -> [Spot]
adjacentSpots x y seatMap = Maybe.mapMaybe (\(x', y') -> spotAt x' y' seatMap) (adjacentCoords x y)

adjacentCoords :: (Num a, Enum a, Eq a) => a -> a -> [(a, a)]
adjacentCoords =
  let deltas = [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]
   in \x y -> [(x + dx, y + dy) | (dx, dy) <- deltas]

stepUntilStable :: Rule -> SeatMap -> SeatMap
stepUntilStable rule seatMap =
  case step rule seatMap of
    nextMap
      | nextMap == seatMap -> nextMap
      | otherwise -> stepUntilStable rule nextMap

step :: Rule -> SeatMap -> SeatMap
step rule seatMap@(SeatMap rows) =
  SeatMap $
    flip Seq.mapWithIndex rows $ \y cols ->
      flip Seq.mapWithIndex cols $ \x currentSpot ->
        maybe currentSpot rule $! seatRegion x y seatMap

toSpotList :: SeatMap -> [Spot]
toSpotList (SeatMap rows) = Foldable.toList . Foldable.fold $ rows

seatRegion :: Int -> Int -> SeatMap -> Maybe SeatRegion
seatRegion x y seatMap = do
  spot <- spotAt x y seatMap
  pure $
    SeatRegion
      { focus = (x, y)
      , spot
      , seatMap
      }

spotAt :: Int -> Int -> SeatMap -> Maybe Spot
spotAt x y (SeatMap rows) = rows Seq.!? y >>= (Seq.!? x)

parseSpot :: Char -> Maybe Spot
parseSpot '.' = Just Floor
parseSpot 'L' = Just Free
parseSpot '#' = Just Occupied
parseSpot _ = Nothing

example :: SeatMap
example =
  SeatMap . Seq.fromList . fmap (Seq.fromList . Maybe.mapMaybe parseSpot) $
    [ "L.LL.LL.LL"
    , "LLLLLLL.LL"
    , "L.L.L..L.."
    , "LLLL.LL.LL"
    , "L.LL.LL.LL"
    , "L.LLLLL.LL"
    , "..L.L....."
    , "LLLLLLLLLL"
    , "L.LLLLLL.L"
    , "L.LLLLL.LL"
    ]

input :: IO SeatMap
input = SeatMap <$> Advent2020.Input.loadInput parse "resources/day11/input"
 where
  parse line = case Seq.fromList . Maybe.mapMaybe parseSpot $ line of
    Seq.Empty -> Nothing
    row -> Just row

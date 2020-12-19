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
import Data.Foldable qualified as Foldable
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
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
-- 25

solve1 :: Seq Op -> Int
solve1 = manhattan . snd . Foldable.foldl' go initialPos . Foldable.toList
 where
  initialPos = (East, (0, 0))
  go :: (Direction, (Int, Int)) -> Op -> (Direction, (Int, Int))
  go (currentDirection, pos) (OpTurn turnOp) = (rotate turnOp currentDirection, pos)
  go (currentDirection, pos) (OpMove (Towards direction amount)) = (currentDirection, moveTo direction amount pos)
  go (currentDirection, pos) (OpMove (Forward amount)) = (currentDirection, moveTo currentDirection amount pos)

-- >>> solve2 example
-- 286

solve2 :: Seq Op -> Int
solve2 = manhattan . snd . Foldable.foldl' go initialPos . Foldable.toList
 where
  initialPos = (Waypoint (10, -1), (0, 0))
  go :: (Waypoint, (Int, Int)) -> Op -> (Waypoint, (Int, Int))
  go (waypoint, pos) (OpTurn turnOp) = (rotateWaypoint turnOp waypoint, pos)
  go (waypoint, pos) (OpMove (Towards direction amount)) = (moveWaypointTo direction amount waypoint, pos)
  go (waypoint, pos) (OpMove (Forward amount)) = (waypoint, toWaypoint amount waypoint pos)

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

newtype Waypoint = Waypoint {unWaypoint :: (Int, Int)}

rotateWaypoint :: Turn -> Waypoint -> Waypoint
rotateWaypoint Around (Waypoint (dx, dy)) = Waypoint (-dx, -dy)
rotateWaypoint Cw (Waypoint (dx, dy)) = Waypoint (-dy, dx)
rotateWaypoint Ccw (Waypoint (dx, dy)) = Waypoint (dy, -dx)

moveWaypointTo :: Direction -> Int -> Waypoint -> Waypoint
moveWaypointTo direction i = Waypoint . moveTo direction i . unWaypoint

toWaypoint :: Int -> Waypoint -> (Int, Int) -> (Int, Int)
toWaypoint times (Waypoint (dx, dy)) (x, y) = (x + times * dx, y + times * dy)

rotate :: Turn -> Direction -> Direction
rotate Cw West = North
rotate Cw d = succ d
rotate Ccw North = West
rotate Ccw d = pred d
rotate Around North = South
rotate Around South = North
rotate Around East = West
rotate Around West = East

moveTo :: Direction -> Int -> (Int, Int) -> (Int, Int)
moveTo direction i (x, y) = (x + dx, y + dy)
 where
  dx = case direction of
    East -> i
    West -> - i
    _ -> 0
  dy = case direction of
    South -> i
    North -> - i
    _ -> 0

data Move
  = Towards Direction Int
  | Forward Int
  deriving stock (Eq, Ord, Show)

data Direction
  = North
  | East
  | South
  | West
  deriving stock (Eq, Ord, Show, Enum)

data Turn = Cw | Ccw | Around
  deriving stock (Eq, Ord, Show)

data Op = OpMove Move | OpTurn Turn
  deriving stock (Eq, Ord, Show)

parseOp :: String -> Maybe Op
parseOp ('N' : digits) = OpMove . Towards North <$> Read.readMaybe digits
parseOp ('S' : digits) = OpMove . Towards South <$> Read.readMaybe digits
parseOp ('E' : digits) = OpMove . Towards East <$> Read.readMaybe digits
parseOp ('W' : digits) = OpMove . Towards West <$> Read.readMaybe digits
parseOp ('F' : digits) = OpMove . Forward <$> Read.readMaybe digits
parseOp string = OpTurn <$> parseTurn string

parseTurn :: String -> Maybe Turn
parseTurn [] = Nothing
parseTurn (d : digits) = do
  direction' <- direction d
  amount <- Read.readMaybe @Int digits
  build direction' amount
 where
  direction 'R' = Just $ Right ()
  direction 'L' = Just $ Left ()
  direction _ = Nothing
  build :: Either () () -> Int -> Maybe Turn
  build _ 180 = Just Around
  build (Right _) 90 = Just Cw
  build (Right _) 270 = Just Ccw
  build (Left _) 90 = Just Ccw
  build (Left _) 270 = Just Cw
  build _ _ = Nothing

example :: Seq Op
example =
  Seq.fromList . Maybe.mapMaybe parseOp $
    [ "F10"
    , "N3"
    , "F7"
    , "R90"
    , "F11"
    ]

input :: IO (Seq Op)
input = Advent2020.Input.loadInput parseOp "resources/day12/input"

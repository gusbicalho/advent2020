{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Advent2020.Input qualified
import Control.Arrow (Arrow ((&&&)))
import Control.Monad (when)
import Control.Monad.Trans.Writer.CPS qualified as WriterT
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show . solve $ example3d
  putStrLn . ("for real: " <>) . show . solve =<< input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show . solve . in4d $ example3d
  putStrLn . ("for real: " <>) . show . solve . in4d =<< input

-- >>> solve1 example3d
-- 112

solve :: Coords coords => World coords -> Int
solve = Set.size . activeCells . (!! 6) . List.iterate' (step adjacentCells activate)
 where
  activate (_, True) neighbors =
    length (filter snd neighbors) `elem` [2, 3]
  activate (_, False) neighbors =
    length (filter snd neighbors) == 3

type NeighborhoodFn coords = coords -> [coords]
type ActivationFn coords = (coords, Bool) -> [(coords, Bool)] -> Bool

step :: Coords coords => NeighborhoodFn coords -> ActivationFn coords -> World coords -> World coords
step neighborhood activate world =
  World . WriterT.execWriter $
    Foldable.for_ (areaAround $ activeCells world) $ \coords -> do
      let currentlyActive = isActive world coords
          neighbors = (id &&& isActive world) <$> neighborhood coords
          becomesActive = activate (coords, currentlyActive) neighbors
      when becomesActive $
        WriterT.tell $ Set.singleton coords

class Ord coords => Coords coords where
  adjacentCells :: coords -> [coords]
  areaAround :: Foldable t => t coords -> [coords]

instance Coords Coords3d where
  areaAround points =
    [ (x, y, z)
    | x <- rangeAround xs
    , y <- rangeAround ys
    , z <- rangeAround zs
    ]
   where
    xs = (\(x, _, _) -> x) <$> Foldable.toList points
    ys = (\(_, y, _) -> y) <$> Foldable.toList points
    zs = (\(_, _, z) -> z) <$> Foldable.toList points
    rangeAround ds = [minimum ds - 1 .. maximum ds + 1]

  adjacentCells (x, y, z) =
    [ (x + dx, y + dy, z + dz)
    | dx <- [-1 .. 1]
    , dy <- [-1 .. 1]
    , dz <- [-1 .. 1]
    , dx /= 0 || dy /= 0 || dz /= 0
    ]

instance Coords Coords4d where
  areaAround points =
    [ (x, y, z, w)
    | x <- rangeAround xs
    , y <- rangeAround ys
    , z <- rangeAround zs
    , w <- rangeAround ws
    ]
   where
    xs = (\(x, _, _, _) -> x) <$> Foldable.toList points
    ys = (\(_, y, _, _) -> y) <$> Foldable.toList points
    zs = (\(_, _, z, _) -> z) <$> Foldable.toList points
    ws = (\(_, _, _, w) -> w) <$> Foldable.toList points
    rangeAround ds = [minimum ds - 1 .. maximum ds + 1]

  adjacentCells (x, y, z, w) =
    [ (x + dx, y + dy, z + dz, w + dw)
    | dx <- [-1 .. 1]
    , dy <- [-1 .. 1]
    , dz <- [-1 .. 1]
    , dw <- [-1 .. 1]
    , dx /= 0 || dy /= 0 || dz /= 0 || dw /= 0
    ]

-- printWorld3d :: World Coords3d -> IO ()
-- printWorld3d world =
--   Foldable.for_ (showWorld3d world) $ \(z, rows) -> do
--     putStrLn $ "z=" <> show z
--     Foldable.traverse_ putStrLn rows
--     putStrLn ""

-- showWorld3d :: World Coords3d -> [(Int, [String])]
-- showWorld3d world@(World activeCells) =
--   flip fmap (fullRange zs) $ \z ->
--     ( z
--     , flip fmap (fullRange ys) $ \y ->
--         flip fmap (fullRange xs) $ \x ->
--           Bool.bool '.' '#' $ isActive world (x, y, z)
--     )
--  where
--   xs = (\(x, _, _) -> x) <$> Foldable.toList activeCells
--   ys = (\(_, y, _) -> y) <$> Foldable.toList activeCells
--   zs = (\(_, _, z) -> z) <$> Foldable.toList activeCells
--   fullRange ds = [minimum ds .. maximum ds]

isActive :: Ord coords => World coords -> coords -> Bool
isActive World{activeCells} coords = coords `Set.member` activeCells

type Coords3d = (Int, Int, Int)
type Coords4d = (Int, Int, Int, Int)
newtype World c = World {activeCells :: Set c}
  deriving stock (Eq, Show)

in4d :: World Coords3d -> World Coords4d
in4d = World . Set.map (\(x,y,z) -> (x,y,z,0)) . activeCells

parseWorld3d :: Int -> Seq String -> World Coords3d
parseWorld3d z = World . Set.fromList . Seq.foldMapWithIndex toLiveCoords
 where
  toLiveCoords :: Int -> String -> [Coords3d]
  toLiveCoords y = Maybe.mapMaybe (toLiveCoord y) . zip [0 ..]
  toLiveCoord y (x, '#') = Just (x, y, z)
  toLiveCoord _ _ = Nothing

example3d :: World Coords3d
example3d =
  parseWorld3d 0 . Seq.fromList $
    [ ".#."
    , "..#"
    , "###"
    ]

input :: IO (World Coords3d)
input = parseWorld3d 0 <$> Advent2020.Input.loadInput Just "day17/input"

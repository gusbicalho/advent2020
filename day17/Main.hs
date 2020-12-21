{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Advent2020.Input qualified
import Data.Bool qualified as Bool
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
  putStrLn "Part 0 (2d)"
  putStrLn . ("example: " <>) . show . solve $ example
  putStrLn . ("for real: " <>) . show . solve =<< input
  putStrLn "Part 1 (3d)"
  putStrLn . ("example: " <>) . show . solve . liftD 0 $ example
  putStrLn . ("for real: " <>) . show . solve . liftD 0 =<< input
  putStrLn "Part 2 (4d)"
  putStrLn . ("example: " <>) . show . solve . liftD 0 . liftD 0 $ example
  putStrLn . ("for real: " <>) . show . solve . liftD 0 . liftD 0 =<< input

solve :: Coords coords => World coords -> Int
solve = Set.size . activeCells . (!! 6) . List.iterate' doStep

doStep :: Coords coords => World coords -> World coords
doStep = step adjacentCells activate
 where
  adjacentCells p = filter (/= p) $ areaAround [p]
  activate True neighbors =
    length (filter id neighbors) `elem` [2, 3]
  activate False neighbors =
    length (filter id neighbors) == 3

type NeighborhoodFn coords = coords -> [coords]
type ActivationFn coords = Bool -> [Bool] -> Bool

step :: Coords coords => NeighborhoodFn coords -> ActivationFn coords -> World coords -> World coords
step neighborhood activate world =
  activeCells world
    & areaAround
    & Maybe.mapMaybe activateCell
    & Set.fromList
    & World
 where
  activateCell coords =
    Bool.bool Nothing (Just coords) $
      activate
        (isActive world coords)
        (isActive world <$> neighborhood coords)

class Ord coords => Coords coords where
  areaAround :: Foldable t => t coords -> [coords]

instance Coords Int where
  areaAround points
    | Foldable.null points = []
    | otherwise = [minimum points - 1 .. maximum points + 1]

instance Coords coords => Coords (coords, Int) where
  areaAround points =
    [ (xyz, w)
    | xyz <- areaAround xyzs
    , w <- areaAround ws
    ]
   where
    xyzs = fst <$> Foldable.toList points
    ws = snd <$> Foldable.toList points

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
--           Bool.bool '.' '#' $ isActive world ((x, y), z)
--     )
--  where
--   xs = (\((x, _), _) -> x) <$> Foldable.toList activeCells
--   ys = (\((_, y), _) -> y) <$> Foldable.toList activeCells
--   zs = (\((_, _), z) -> z) <$> Foldable.toList activeCells
--   fullRange ds = [minimum ds .. maximum ds]

isActive :: Ord coords => World coords -> coords -> Bool
isActive World{activeCells} coords = coords `Set.member` activeCells

type Coords2d = (Int, Int)
newtype World c = World {activeCells :: Set c}
  deriving stock (Eq, Show)

liftD :: Ord coords => Int -> World coords -> World (coords, Int)
liftD w = World . Set.map (,w) . activeCells

parseWorld2d :: Seq String -> World Coords2d
parseWorld2d = World . Set.fromList . Seq.foldMapWithIndex toLiveCoords
 where
  toLiveCoords :: Int -> String -> [Coords2d]
  toLiveCoords y = Maybe.mapMaybe (toLiveCoord y) . zip [0 ..]
  toLiveCoord y (x, '#') = Just (x, y)
  toLiveCoord _ _ = Nothing

example :: World Coords2d
example =
  parseWorld2d . Seq.fromList $
    [ ".#."
    , "..#"
    , "###"
    ]

input :: IO (World Coords2d)
input = parseWorld2d <$> Advent2020.Input.loadInput Just "day17/input"

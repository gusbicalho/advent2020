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
import Data.List qualified as List
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show $ solve1 example
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show $ solve2 example
  putStrLn . ("for real: " <>) . show . solve2 =<< input

solve1 :: Seq Step -> Int
solve1 = Set.size . blackTiles . initialState

solve2 :: Seq Step -> Int
solve2 = Set.size . blackTiles . (!! 100) . List.iterate' aDayPasses . initialState

newtype Floor = Floor {blackTiles :: Set Coord}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)
data Coord = Coord {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)
data Step = Step {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

(!+) :: Coord -> Step -> Coord
(Coord x y) !+ (Step dx dy) = Coord (x + dx) (y + dy)

instance Semigroup Step where
  Step x1 y1 <> Step x2 y2 = Step (x1 + x2) (y1 + y2)

instance Monoid Step where
  mempty = Step 0 0

initialState :: Foldable t => t Step -> Floor
initialState = Floor . Foldable.foldl' flipTile mempty
 where
  origin = Coord 0 0
  flipTile blackTiles step = toggle (origin !+ step) blackTiles
  toggle a set
    | a `Set.member` set = Set.delete a set
    | otherwise = Set.insert a set

aDayPasses :: Floor -> Floor
aDayPasses (Floor blackTiles) = Floor $ Set.filter shouldBeBlack relevantTiles
 where
  relevantTiles :: Set Coord
  relevantTiles = Set.fromList . concatMap (\c -> c : adjacent c) . Foldable.toList $ blackTiles
  shouldBeBlack :: Coord -> Bool
  shouldBeBlack coord
    | coord `Set.member` blackTiles =
      let b = numberOfAdjacentBlackTiles coord
       in 0 < b && b <= 2
    | otherwise = 2 == numberOfAdjacentBlackTiles coord
  numberOfAdjacentBlackTiles = length . filter (`Set.member` blackTiles) . adjacent

adjacent :: Coord -> [Coord]
adjacent c =
  (c !+)
    <$> [ east
        , southeast
        , southwest
        , west
        , northwest
        , northeast
        ]

parseStep :: String -> Step
parseStep = Foldable.fold . go
 where
  go ('s' : 'e' : cs) = southeast : go cs
  go ('s' : 'w' : cs) = southwest : go cs
  go ('n' : 'e' : cs) = northeast : go cs
  go ('n' : 'w' : cs) = northwest : go cs
  go ('e' : cs) = east : go cs
  go ('w' : cs) = west : go cs
  go _ = []

east :: Step
east = Step 1 1
southeast :: Step
southeast = Step 0 1
southwest :: Step
southwest = Step (-1) 0
west :: Step
west = Step (-1) (-1)
northwest :: Step
northwest = Step 0 (-1)
northeast :: Step
northeast = Step 1 0

example :: Seq Step
example =
  Seq.fromList . fmap parseStep $
    [ "sesenwnenenewseeswwswswwnenewsewsw"
    , "neeenesenwnwwswnenewnwwsewnenwseswesw"
    , "seswneswswsenwwnwse"
    , "nwnwneseeswswnenewneswwnewseswneseene"
    , "swweswneswnenwsewnwneneseenw"
    , "eesenwseswswnenwswnwnwsewwnwsene"
    , "sewnenenenesenwsewnenwwwse"
    , "wenwwweseeeweswwwnwwe"
    , "wsweesenenewnwwnwsenewsenwwsesesenwne"
    , "neeswseenwwswnwswswnw"
    , "nenwswwsewswnenenewsenwsenwnesesenew"
    , "enewnwewneswsewnwswenweswnenwsenwsw"
    , "sweneswneswneneenwnewenewwneswswnese"
    , "swwesenesewenwneswnwwneseswwne"
    , "enesenwswwswneneswsenwnewswseenwsese"
    , "wnwnesenesenenwwnenwsewesewsesesew"
    , "nenewswnwewswnenesenwnesewesw"
    , "eneswnwswnwsenenwnwnwwseeswneewsenese"
    , "neswnwewnwnwseenwseesewsenwsweewe"
    , "wseweeenwnesenwwwswnew"
    ]

input :: IO (Seq Step)
input = Advent2020.Input.loadInput (Just . parseStep) "day24/input"

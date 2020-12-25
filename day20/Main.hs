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
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Text.Read qualified as Read
import Control.Monad ((>=>))

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show $ solve1 example
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show $ solve2 example
  putStrLn . ("for real: " <>) . show . solve2 =<< input

solve1 :: Seq Tile -> Word
solve1 = maybe 0 (product . fmap tileId . fourCorners) . solveTiles . Foldable.toList

solve2 :: Seq a -> Int
solve2 = Seq.length

solveTiles :: [Tile] -> Maybe (Seq (Seq Tile))
solveTiles tiles = toTable . verticals . fmap pure . horizontals . fmap pure $ tiles
 where
  squareSize = fromIntegral $ sqrtWord (List.genericLength @Word tiles)
  emptyTable = Seq.replicate squareSize . Seq.replicate squareSize $ Nothing
  -- Ah, fro part 1 at least we only need to find corners, not solve puzzle
  go :: Seq (Seq (Maybe Tile)) -> [Tile] ->
  horizontals :: [[Tile]] -> [[Tile]]
  horizontals horizontalSpans = loopUntilStable (concatMatching matchHorizontally)
  verticals :: [[[Tile]]] -> [[[Tile]]]
  verticals = _
  toTable :: [[[Tile]]] -> Maybe (Seq (Seq Tile))
  toTable [square] = Just . Seq.fromList $ Seq.fromList <$> square
  toTable _ = Nothing


sqrtWord :: Word -> Word
sqrtWord 0 = 0
sqrtWord 1 = 1
sqrtWord n = go 2
 where
  go r | r * r >= n = r
       | otherwise = go (r + 1)

fourCorners :: Seq (Seq a) -> [a]
fourCorners = firstAndLast >=> firstAndLast
 where
  firstAndLast (f Seq.:<| (_ Seq.:|> l)) = [f, l]
  firstAndLast _ = []

flips :: Tile -> [Tile]
flips tile = fmap ($ tile) [flipX, flipY, rotateCw, rotate180, rotateCcw]

flipX :: Tile -> Tile
flipX tile@Tile { tileData } = tile { tileData = fmap Seq.reverse tileData }

flipY :: Tile -> Tile
flipY tile@Tile { tileData } = tile { tileData = Seq.reverse tileData }

transpose :: Tile -> Tile
transpose tile@Tile{tileData} = tile { tileData = transposed }
 where
  transposed =
    Seq.fromList . flip fmap [0..9] $ \y ->
      Seq.fromList . flip fmap [0..9] $ \x ->
        tileData & (Seq.!? x) >>= (Seq.!? y) & Maybe.fromMaybe False

rotate180 :: Tile -> Tile
rotate180 = flipX . flipY

rotateCw :: Tile -> Tile
rotateCw  = flipX . transpose

rotateCcw :: Tile -> Tile
rotateCcw  = flipY . transpose

data Tile = Tile {tileId :: Word, tileData :: Seq (Seq Bool)}
  deriving stock (Eq, Ord, Show)

parseTiles :: Foldable t => t String -> [Tile]
parseTiles = readTiles . Foldable.toList
 where
  readTiles :: [String] -> [Tile]
  readTiles = Maybe.mapMaybe readTile . splitWhen (== "")
  readTile :: [String] -> Maybe Tile
  readTile [] = Nothing
  readTile (s : ss) =
    readTileId s <&> \tileId ->
      Tile
        { tileId
        , tileData = Seq.fromList (Seq.fromList . fmap pixelToBit <$> ss)
        }
  readTileId s =
    s & List.stripPrefix "Tile "
      <&> takeWhile Char.isDigit
      >>= Read.readMaybe @Word
  pixelToBit '#' = True
  pixelToBit _ = False

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = case break p (dropWhile p xs) of
  ([], _) -> []
  (ys, xs') -> ys : splitWhen p xs'

example :: Seq Tile
example =
  Seq.fromList . parseTiles $
    [ "Tile 2311:"
    , "..##.#..#."
    , "##..#....."
    , "#...##..#."
    , "####.#...#"
    , "##.##.###."
    , "##...#.###"
    , ".#.#.#..##"
    , "..#....#.."
    , "###...#.#."
    , "..###..###"
    , ""
    , "Tile 1951:"
    , "#.##...##."
    , "#.####...#"
    , ".....#..##"
    , "#...######"
    , ".##.#....#"
    , ".###.#####"
    , "###.##.##."
    , ".###....#."
    , "..#.#..#.#"
    , "#...##.#.."
    , ""
    , "Tile 1171:"
    , "####...##."
    , "#..##.#..#"
    , "##.#..#.#."
    , ".###.####."
    , "..###.####"
    , ".##....##."
    , ".#...####."
    , "#.##.####."
    , "####..#..."
    , ".....##..."
    , ""
    , "Tile 1427:"
    , "###.##.#.."
    , ".#..#.##.."
    , ".#.##.#..#"
    , "#.#.#.##.#"
    , "....#...##"
    , "...##..##."
    , "...#.#####"
    , ".#.####.#."
    , "..#..###.#"
    , "..##.#..#."
    , ""
    , "Tile 1489:"
    , "##.#.#...."
    , "..##...#.."
    , ".##..##..."
    , "..#...#..."
    , "#####...#."
    , "#..#.#.#.#"
    , "...#.#.#.."
    , "##.#...##."
    , "..##.##.##"
    , "###.##.#.."
    , ""
    , "Tile 2473:"
    , "#....####."
    , "#..#.##..."
    , "#.##..#..."
    , "######.#.#"
    , ".#...#.#.#"
    , ".#########"
    , ".###.#..#."
    , "########.#"
    , "##...##.#."
    , "..###.#.#."
    , ""
    , "Tile 2971:"
    , "..#.#....#"
    , "#...###..."
    , "#.#.###..."
    , "##.##..#.."
    , ".#####..##"
    , ".#..####.#"
    , "#..#.#..#."
    , "..####.###"
    , "..#.#.###."
    , "...#.#.#.#"
    , ""
    , "Tile 2729:"
    , "...#.#.#.#"
    , "####.#...."
    , "..#.#....."
    , "....#..#.#"
    , ".##..##.#."
    , ".#.####..."
    , "####.#.#.."
    , "##.####..."
    , "##..#.##.."
    , "#.##...##."
    , ""
    , "Tile 3079:"
    , "#.#.#####."
    , ".#..######"
    , "..#......."
    , "######...."
    , "####.#..#."
    , ".#...#.##."
    , "#.#####.##"
    , "..#.###..."
    , "..#......."
    , "..#.###..."
    ]

input :: IO (Seq Tile)
input = Seq.fromList . parseTiles <$> Advent2020.Input.loadInput Just "dayTemplate/input"

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import Control.Monad (guard, (>=>))
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
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

solve1 :: Seq Tile -> Word
solve1 = maybe 0 (product . fmap tileId . fourCorners) . solveTiles . Foldable.toList

solve2 :: Seq a -> Int
solve2 = Seq.length

solveTiles :: [Tile] -> Maybe (Seq (Seq Tile))
solveTiles tiles = do
  (corner, _) <- List.find isCorner matchList
  let remaining = remove (sameTileIdAs corner . fst) matchList
      (borderRow, remaining') = goRow (ignoringOrder matchH) [] corner remaining
      (cols, remaining'') = goCols [] borderRow remaining'
  guard (null remaining'')
  pure $ Seq.fromList $ Seq.fromList <$> cols
 where
  remove p = filter (not . p)
  sameTileIdAs t = (== tileId t) . tileId
  goRow matcher rowTiles headTile possibleTiles =
    case Maybe.mapMaybe (checkForMatch matcher headTile) possibleTiles of
      [] -> (reverse $ headTile : rowTiles, possibleTiles)
      (t : _) -> goRow matcher (headTile : rowTiles) t (remove (sameTileIdAs t . fst) possibleTiles)
  checkForMatch matcher t (t2, t2matches) =
    if any (sameTileIdAs t) t2matches
      then lookForMatch matcher t t2
      else Nothing
  goCols acc [] possibleTiles = (acc, possibleTiles)
  goCols acc (colHead : moreColHeads) possibleTiles =
    let (col, remaining) = goRow (ignoringOrder matchV) [] colHead possibleTiles
     in goCols (col : acc) moreColHeads remaining
  isCorner (_, [_, _]) = True
  isCorner _ = False
  matchList :: [(Tile, [Tile])]
  matchList = fmap (id &&& matchesFor) tiles
  matchesFor tile =
    Maybe.mapMaybe (lookForMatch (ignoringOrder matchAny) tile)
      . remove (sameTileIdAs tile)
      $ tiles
  lookForMatch matcher tg1 tg2 =
    Maybe.listToMaybe $ do
      tg2' <- flips tg2
      guard (matcher tg1 tg2')
      pure tg2'
  ignoringOrder matcher t1 t2 = matcher t1 t2 || matcher t2 t1
  matchAny t1 t2 = matchV t1 t2 || matchH t1 t2

fourCorners :: Seq (Seq a) -> [a]
fourCorners = firstAndLast >=> firstAndLast
 where
  firstAndLast (f Seq.:<| (_ Seq.:|> l)) = [f, l]
  firstAndLast _ = []

matchH :: Tile -> Tile -> Bool
matchH Tile{tileData = td1} Tile{tileData = td2} =
  Maybe.mapMaybe seqLast (Foldable.toList td1) == Maybe.mapMaybe seqFirst (Foldable.toList td2)
 where
  seqLast (_ Seq.:|> l) = Just l
  seqLast _ = Nothing
  seqFirst (f Seq.:<| _) = Just f
  seqFirst _ = Nothing

matchV :: Tile -> Tile -> Bool
matchV Tile{tileData = td1} Tile{tileData = td2} =
  seqLast td1 == seqFirst td2
 where
  seqLast (_ Seq.:|> l) = Just l
  seqLast _ = Nothing
  seqFirst (f Seq.:<| _) = Just f
  seqFirst _ = Nothing

flipX :: Tile -> Tile
flipX tile@Tile{tileData} = tile{tileData = fmap Seq.reverse tileData}

flipY :: Tile -> Tile
flipY tile@Tile{tileData} = tile{tileData = Seq.reverse tileData}

transpose :: Tile -> Tile
transpose tile@Tile{tileData} = tile{tileData = transposed}
 where
  transposed =
    Seq.fromList . flip fmap [0 .. 9] $ \y ->
      Seq.fromList . flip fmap [0 .. 9] $ \x ->
        tileData & (Seq.!? x) >>= (Seq.!? y) & Maybe.fromMaybe False

flips :: Tile -> [Tile]
flips tile = fmap ($ tile) [id, flipX, flipY, rotateCw, rotate180, rotateCcw, transpose, transposeFlipped]

transposeFlipped :: Tile -> Tile
transposeFlipped = rotate180 . transpose

rotate180 :: Tile -> Tile
rotate180 = flipX . flipY

rotateCw :: Tile -> Tile
rotateCw = flipX . transpose

rotateCcw :: Tile -> Tile
rotateCcw = flipY . transpose

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
input = Seq.fromList . parseTiles <$> Advent2020.Input.loadInput Just "day20/input"

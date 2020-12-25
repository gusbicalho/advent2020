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
  let (borderRow, remaining) = goRow (ignoringOrder matchH) [] corner (remove (sameTileIdAs corner . fst) matchList)
  let (cols, remaining') = goCols [] borderRow remaining
  guard (null remaining')
  pure $ Seq.fromList $ Seq.fromList <$> cols
 where
  remove p = filter (not . p)
  sameTileIdAs t = (== tileId t) . tileId
  goRow matcher rowTiles headTile possibleTiles =
    case Maybe.mapMaybe (checkForMatch matcher headTile) possibleTiles of
      [] -> (reverse $ headTile : rowTiles, possibleTiles)
      (t : _) -> goRow matcher (headTile : rowTiles) t (remove (sameTileIdAs t . fst) possibleTiles)
  checkForMatch matcher t (t2, (t2up, t2right, t2down, t2left)) =
    if any (sameTileIdAs t) $ Maybe.catMaybes [t2up, t2right, t2down, t2left]
      then lookForMatch matcher t t2
      else Nothing
  goCols acc [] possibleTiles = (acc, possibleTiles)
  goCols acc (colHead : moreColHeads) possibleTiles =
    let (col, remaining) = goRow (ignoringOrder matchV) [] colHead possibleTiles
     in goCols (col : acc) moreColHeads remaining
  isCorner (_, (up, right, down, left)) =
    (Maybe.isJust up /= Maybe.isJust down) && (Maybe.isJust left /= Maybe.isJust right)
  matchList :: [(Tile, (Maybe Tile, Maybe Tile, Maybe Tile, Maybe Tile))]
  matchList = fmap (id &&& matchesFor) tiles
  ignoringOrder matcher t1 t2 = matcher t1 t2 || matcher t2 t1
  matchesFor tile =
    ( Maybe.listToMaybe $ Maybe.mapMaybe (lookForMatch (flip matchV) tile) $ remove (sameTileIdAs tile) tiles
    , Maybe.listToMaybe $ Maybe.mapMaybe (lookForMatch matchH tile) $ remove (sameTileIdAs tile) tiles
    , Maybe.listToMaybe $ Maybe.mapMaybe (lookForMatch matchV tile) $ remove (sameTileIdAs tile) tiles
    , Maybe.listToMaybe $ Maybe.mapMaybe (lookForMatch (flip matchH) tile) $ remove (sameTileIdAs tile) tiles
    )
  lookForMatch matcher tg1 tg2 =
    Maybe.listToMaybe $ do
      tg2' <- flips tg2
      guard (matcher tg1 tg2')
      pure tg2'

fourCorners :: Seq (Seq a) -> [a]
fourCorners = firstAndLast >=> firstAndLast
 where
  firstAndLast (f Seq.:<| (_ Seq.:|> l)) = [f, l]
  firstAndLast _ = []

instance Piece Tile where
  matchH Tile{tileData = td1} Tile{tileData = td2} =
    Maybe.mapMaybe seqLast (Foldable.toList td1) == Maybe.mapMaybe seqFirst (Foldable.toList td2)
   where
    seqLast (_ Seq.:|> l) = Just l
    seqLast _ = Nothing
    seqFirst (f Seq.:<| _) = Just f
    seqFirst _ = Nothing

  matchV Tile{tileData = td1} Tile{tileData = td2} =
    seqLast td1 == seqFirst td2
   where
    seqLast (_ Seq.:|> l) = Just l
    seqLast _ = Nothing
    seqFirst (f Seq.:<| _) = Just f
    seqFirst _ = Nothing

  flipX tile@Tile{tileData} = tile{tileData = fmap Seq.reverse tileData}

  flipY tile@Tile{tileData} = tile{tileData = Seq.reverse tileData}

  transpose tile@Tile{tileData} = tile{tileData = transposed}
   where
    transposed =
      Seq.fromList . flip fmap [0 .. 9] $ \y ->
        Seq.fromList . flip fmap [0 .. 9] $ \x ->
          tileData & (Seq.!? x) >>= (Seq.!? y) & Maybe.fromMaybe False

class Piece p where
  matchH :: p -> p -> Bool
  matchV :: p -> p -> Bool
  flipX :: p -> p
  flipY :: p -> p
  transpose :: p -> p

flips :: Piece p => p -> [p]
flips tile = fmap ($ tile) [id, flipX, flipY, rotateCw, rotate180, rotateCcw, transpose, transposeFlipped]

transposeFlipped :: Piece p => p -> p
transposeFlipped = rotate180 . transpose

rotate180 :: Piece p => p -> p
rotate180 = flipX . flipY

rotateCw :: Piece p => p -> p
rotateCw = flipX . transpose

rotateCcw :: Piece p => p -> p
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

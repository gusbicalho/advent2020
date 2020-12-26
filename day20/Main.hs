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
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Advent2020.Input qualified
import Control.Arrow (Arrow ((&&&)))
import Control.Monad (guard, (>=>))
import Data.Bifunctor (Bifunctor(bimap))
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Text.Read qualified as Read
import Data.Ord (comparing)

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show $ solve1 example
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show =<< solve2 example
  putStrLn . ("for real: " <>) . show =<< solve2 =<< input

solve1 :: Seq Tile -> Word
solve1 = maybe 0 (product . fmap tileId . fourCorners) . solveTiles . Foldable.toList
 where
  fourCorners = firstAndLast >=> firstAndLast
  firstAndLast (f Seq.:<| (_ Seq.:|> l)) = [f, l]
  firstAndLast _ = []

solve2 :: Foldable t => t Tile -> IO Word
solve2 tiles = do
  let rendered = renderImage . Maybe.fromJust . solveTiles . Foldable.toList $ tiles
      flipped = fmap tileData . flips . Tile 0 $ rendered
      (dragonCount, selected) = List.maximumBy (comparing fst) $ (countSeaDragons &&& id) <$> flipped
  putStrLn "Selected:"
  printPixels selected
  pure $ countChars '#' selected - 15 * dragonCount

printPixels :: (Foldable t, Foldable u) => t (u Char) -> IO ()
printPixels =
  Foldable.traverse_ $ \row ->
    putStrLn $ Foldable.toList row

countChars :: Char -> Seq (Seq Char) -> Word
countChars c = sumMap (sumMap (\c' -> if c' == c then 1 :: Word else 0))
 where
  sumMap f = sum . fmap f

countSeaDragons :: Seq (Seq Char) -> Word
countSeaDragons pixels =
  List.genericLength $ filter isSeaDragon (coords pixels)
 where
  coords pxs =
    let height = length pxs
        width = maybe 0 length (pxs Seq.!? 0)
     in [(x, y) | y <- [0 .. height -1], x <- [0 .. width -1]]
  (!) :: Seq (Seq Char) -> (Int, Int) -> Char
  (!) s (x, y) = Maybe.fromMaybe ' ' (s Seq.!? y >>= (Seq.!? x))
  isSeaDragon :: (Int, Int) -> Bool
  isSeaDragon (x, y) =
    let coords = bimap (x +) (y +) <$> dragonCoords
    in all (('#' ==) . (pixels !)) coords
  dragonCoords :: [(Int, Int)]
  dragonCoords = filter (('#' ==) . (dragon !)) (coords dragon)
  dragon = Seq.fromList
              [ Seq.fromList "                  # "
              , Seq.fromList "#    ##    ##    ###"
              , Seq.fromList " #  #  #  #  #  #   "
              ]

renderImage :: Seq (Seq Tile) -> Seq (Seq Char)
renderImage = concatWith joinH . Seq.reverse . fmap (concatWith joinV . fmap renderTile)
 where
  renderTile = fmap dropEndpoints . dropEndpoints . tileData
  dropEndpoints (_ Seq.:<| (middle Seq.:|> _)) = middle
  dropEndpoints xs = xs
  concatWith _ Seq.Empty = Seq.Empty
  concatWith _ (pixels Seq.:<| Seq.Empty) = pixels
  concatWith joinFn (pixels Seq.:<| morePixels) = joinFn pixels (concatWith joinFn morePixels)
  joinH = Seq.zipWith (<>)
  joinV top bottom = transposePixels $ joinH (transposePixels top) (transposePixels bottom)

solveTiles :: [Tile] -> Maybe (Seq (Seq Tile))
solveTiles tiles = do
  let remainingMatchables = Map.fromList $ fmap (tileId &&& (id &&& matchesFor)) tiles
  (remainingMatchables, corner) <- pickOneCorner remainingMatchables
  cols <- Foldable.asum . flip fmap (flips corner) $ \corner -> do
    (remainingMatchables, borderRow) <- pure $ goRow matchH [] corner remainingMatchables
    (remainingMatchables, cols) <- pure $ goCols [] borderRow remainingMatchables
    guard (null remainingMatchables)
    Just cols
  pure $ Seq.fromList $ Seq.fromList <$> cols
 where
  goRow matcher rowTiles headTile possibleTiles =
    case Maybe.mapMaybe (checkForMatch matcher headTile) $ Foldable.toList possibleTiles of
      [] -> (possibleTiles, reverse $ headTile : rowTiles)
      (t : _) -> goRow matcher (headTile : rowTiles) t (Map.delete (tileId t) possibleTiles)
  goCols acc [] possibleTiles = (possibleTiles, acc)
  goCols acc (colHead : moreColHeads) possibleTiles =
    let (remaining, col) = goRow matchV [] colHead possibleTiles
     in goCols (col : acc) moreColHeads remaining
  pickOneCorner m = case List.find (isCorner . snd) $ Map.toList m of
    Nothing -> Nothing
    Just (k, (t, _)) -> Just (Map.delete k m, t)
  isCorner (_, [_, _]) = True
  isCorner _ = False
  checkForMatch matcher t (t2, t2matches) =
    if any (sameTileIdAs t) t2matches
      then lookForMatch matcher t t2
      else Nothing
  matchesFor tile =
    Maybe.mapMaybe (lookForMatch (ignoringOrder matchAny) tile)
      . filter (not . sameTileIdAs tile)
      $ tiles
  lookForMatch matcher tg1 tg2 =
    Maybe.listToMaybe $ do
      tg2' <- flips tg2
      guard (matcher tg1 tg2')
      pure tg2'
  sameTileIdAs t = (== tileId t) . tileId
  ignoringOrder matcher t1 t2 = matcher t1 t2 || matcher t2 t1
  matchAny t1 t2 = matchV t1 t2 || matchH t1 t2

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
  transposed = transposePixels tileData

transposePixels :: Seq (Seq Char) -> Seq (Seq Char)
transposePixels =
  Seq.fromList . fmap Seq.fromList . List.transpose . Foldable.toList . fmap Foldable.toList

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

data Tile = Tile {tileId :: Word, tileData :: Seq (Seq Char)}
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
        , tileData = Seq.fromList (Seq.fromList <$> ss)
        }
  readTileId s =
    s & List.stripPrefix "Tile "
      <&> takeWhile Char.isDigit
      >>= Read.readMaybe @Word

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

{-# LANGUAGE ImportQualifiedPost #-}

module Advent2020.Day03 (day03_01, day03_02) where

import Advent2020.Input qualified
import Control.Exception (throwIO)
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Numeric.Natural (Natural)
import Data.Foldable (Foldable(foldl'))
import Data.Functor ((<&>))

day03_01 :: IO Natural
day03_01 = loadGame <&> solve01 (Step 3 1)

-- >>> solve01 (Step 3 1) example
-- 7

-- >>> day03_01
-- 232

solve01 :: Step -> GameState -> Natural
solve01 step gameState = StateT.execState (go gameState) 0
 where
  go gameState = case move step gameState of
    Nothing -> return ()
    Just gameState -> do
      case currentSlot gameState of
        Tree -> StateT.modify' (1 +)
        _ -> return ()
      go gameState

day03_02 :: IO Natural
day03_02 = loadGame <&> solve02 [Step 1 1, Step 3 1, Step 5 1, Step 7 1, Step 1 2]

-- >>> solve02 [Step 1 1, Step 3 1, Step 5 1, Step 7 1, Step 1 2] example
-- 336

-- >>> day03_02
-- 3952291680

solve02 :: [Step] -> GameState -> Natural
solve02 steps gameState = foldl' (*) 1 $ solve01 <$> steps <*> [gameState]

data Slot = Open | Tree
newtype Row = Row {unRow :: Seq Slot}
data GameMap = GameMap
  { gmXSize :: Natural
  , gmYSize :: Natural
  , gmRows :: Seq Row
  }
data Position = Position Natural Natural
data Step = Step Natural Natural
data GameState = GameState GameMap Position

initialPosition :: Position
initialPosition = Position 0 0

currentSlot :: GameState -> Slot
currentSlot (GameState (GameMap _ _ rows) (Position x y)) =
  rows & (`Seq.index` fromIntegral y)
    & unRow
    & (`Seq.index` fromIntegral x)

move :: Step -> GameState -> Maybe GameState
move
  (Step deltaX deltaY)
  ( GameState
      gameMap@(GameMap xSize ySize _)
      (Position currentX currentY)
    )
    | currentY + deltaY >= ySize = Nothing
    | otherwise = Just $ GameState gameMap finalPosition
   where
    finalPosition =
      Position
        ((currentX + deltaX) `mod` xSize)
        (currentY + deltaY)

example :: GameState
example = fromJust $ do
  gameRows <- traverse parseRow exampleLines
  gameMap <- rowsToMap $ Seq.fromList gameRows
  pure $ GameState gameMap initialPosition
 where
  exampleLines =
    [ "..##......."
    , "#...#...#.."
    , ".#....#..#."
    , "..#.#...#.#"
    , ".#...##..#."
    , "..#.##....."
    , ".#.#.#....#"
    , ".#........#"
    , "#.##...#..."
    , "#...##....#"
    , ".#..#...#.#"
    ]

loadGame :: IO GameState
loadGame = do
  gameRows <- Advent2020.Input.loadInput parseRow "resources/day03/input"
  case rowsToMap gameRows of
    Nothing -> throwIO $ userError "Bad map."
    Just gameMap -> pure $ GameState gameMap initialPosition

parseRow :: String -> Maybe Row
parseRow = fmap Row . traverse parseSlot . Seq.fromList
 where
  parseSlot :: Char -> Maybe Slot
  parseSlot '.' = Just Open
  parseSlot '#' = Just Tree
  parseSlot _ = Nothing

rowsToMap :: Seq Row -> Maybe GameMap
rowsToMap rows@(Row firstRowSlots Seq.:<| moreRows)
  | all haveTheSameWidth moreRows =
    Just $ GameMap (fromIntegral width) (fromIntegral height) rows
 where
  width = Seq.length firstRowSlots
  height = Seq.length rows
  haveTheSameWidth (Row slots) = width == Seq.length slots
rowsToMap _ = Nothing

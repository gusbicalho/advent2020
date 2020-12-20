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

import Control.Arrow (Arrow ((&&&)))
import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq

main :: IO ()
main = do
  putStrLn "Part 1"
  Foldable.for_ examples $ \example ->
    putStrLn . ("example " <>) . show . (Foldable.toList &&& solve1) $ example
  putStrLn . ("for real: " <>) . show . solve1 $ input
  putStrLn "Part 2"
  putStrLn . ("for real: " <>) . show . solve2 $ input

solve1 :: Seq Word -> Word
solve1 = solveWithCountdown (2020 - 1)

solve2 :: Seq Word -> Word
solve2 = solveWithCountdown (30000000 - 1)

-- >>> solveWithCountdown (2020 - 1) . Seq.fromList $ [0,3,6]
-- 436
-- main:   131.37s user 76.29s system 239% cpu 1:26.76 total
solveWithCountdown :: Int -> Seq Word -> Word
solveWithCountdown ix ws = case loadPrefix ws of
  Nothing -> error "empty prefix"
  Just prefix -> snd $ runTimes (ix + 1 - Seq.length ws) step prefix
 where
  runTimes :: Int -> (a -> a) -> a -> a
  runTimes !n f zero
    | n <= 0 = zero
    | otherwise = runTimes (pred n) f (f zero)

-- >>> solveWithIterate (2020 - 1) . Seq.fromList $ [0,3,6]
-- 436
-- Should be the same as solveWithCountdown due to lazyness,
-- but it's actually much worse ¯\_(ツ)_/¯
--   324.97s user 270.48s system 311% cpu 3:11.11 total
solveWithIterate :: Int -> Seq Word -> Word
solveWithIterate ix ws = case loadPrefix ws of
  Nothing -> error "empty prefix"
  Just prefix -> snd . (!! (ix + 1 - Seq.length ws)) . iterate step $ prefix

step :: ((Word, Map Word Word), Word) -> ((Word, Map Word Word), Word)
step ((!index, !memory), !latest) =
  let nextState = (succ index, Map.insert latest index memory)
    in case Map.lookup latest memory of
        Nothing -> (nextState, 0)
        Just previousIndex -> (nextState, index - previousIndex)

loadPrefix :: Seq Word -> Maybe ((Word, Map Word Word), Word)
loadPrefix = \case
  Seq.Empty -> Nothing
  x :<| xs -> Just $ goPrefix ((1, Map.empty), x) xs
 where
  goPrefix state Seq.Empty = state
  goPrefix ((!index, !memory), !latest) (x :<| xs) =
    goPrefix ((succ index, Map.insert latest index memory), x) xs

examples :: [Seq Word]
examples =
  [ Seq.fromList [0, 3, 6]
  , Seq.fromList [1, 3, 2]
  , Seq.fromList [2, 1, 3]
  , Seq.fromList [1, 2, 3]
  , Seq.fromList [2, 3, 1]
  , Seq.fromList [3, 2, 1]
  , Seq.fromList [3, 1, 2]
  ]

input :: Seq Word
input = Seq.fromList [20, 9, 11, 0, 1, 2]

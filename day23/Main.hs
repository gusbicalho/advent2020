{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import CircleST (CircleST)
import CircleST qualified
import Control.Arrow ((>>>))
import Control.Monad.ST.Strict (ST)
import Control.Monad.ST.Strict qualified as ST
import Data.Either qualified as Either
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Word (Word64)

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show . solve1' $ example
  putStrLn . ("for real: " <>) . show . solve1' $ input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show $ solve2 example
  putStrLn . ("for real: " <>) . show $ solve2 input

solve1' :: Seq Word64 -> String
solve1' (Seq.take 9 -> ns) =
  ST.runST $
    CircleST.seed (Foldable.toList ns) >>= \case
      Left msg -> pure msg
      Right circle -> do
        getAnswer =<< iterateM (100 :: Int) moveST circle
 where
  getAnswer :: CircleST s Word64 -> ST s String
  getAnswer =
    CircleST.focusOn 1 >>> \case
      Left msg -> pure msg
      Right circle -> do
        Foldable.foldMap' show . drop 1 <$> CircleST.toListCw circle

iterateM :: forall n m a. (Ord n, Num n, Monad m) => n -> (a -> m a) -> a -> m a
iterateM times step = go times
 where
  go :: n -> a -> m a
  go n a
    | n <= 0 = pure a
    | otherwise = go (n - 1) =<< step a

solve2 :: Seq Word64 -> Word64
solve2 ns =
  ST.runST $
    CircleST.seed (Foldable.toList ns) >>= \case
      Left _ -> pure (error "bad seed!")
      Right circle -> do
        getAnswer =<< iterateM (10_000_000 :: Int) moveST circle
 where
  getAnswer circle =
    circle
      & CircleST.focusOn 1
      & Either.fromRight circle
      & CircleST.takeCw 3
      <&> product

moveST :: CircleST s Word64 -> ST s (CircleST s Word64)
moveST circle = do
  let currentLabel = CircleST.focus circle
  extracted <- CircleST.takeCw 3 =<< CircleST.shiftCw circle
  Foldable.for_ extracted $ \label ->
    CircleST.disconnect label circle
  let destination = destinationLabel extracted currentLabel
  Foldable.for_ (reverse extracted) $ \label ->
    CircleST.connectCwOf label destination circle
  CircleST.shiftCw circle
 where
  destinationLabel extracted currentLabel =
    head . dropWhile (`elem` extracted) . drop 1 . List.iterate' labelMinus1 $ currentLabel
  labelMinus1 l
    | l <= CircleST.minimum circle = CircleST.maximum circle
    | otherwise = l - 1

example :: Seq Word64
example =
  Seq.fromList $
    [3, 8, 9, 1, 2, 5, 4, 6, 7] <> [10 .. 1_000_000]

input :: Seq Word64
input =
  Seq.fromList $
    [2, 1, 5, 6, 9, 4, 7, 8, 3] <> [10 .. 1_000_000]

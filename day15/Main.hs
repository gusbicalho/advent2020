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
import Control.Monad ((>=>))
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "Part 1"
  Foldable.for_ examples $ \example ->
    putStrLn . ("example " <>) . show . (Foldable.toList &&& solve1) $ example
  putStrLn . ("for real: " <>) . show . solve1 $ input
  putStrLn "Part 2"
  Foldable.for_ examples $ \example -> do
    putStrLn . ("example " <>) . show . (Foldable.toList &&& solve2) $ example
    hFlush stdout
  putStrLn . ("for real: " <>) . show . solve2 $ input

-- >>> (id &&& solve1) <$> examples
-- [(fromList [0,3,6],436),(fromList [1,3,2],1),(fromList [2,1,3],10),(fromList [1,2,3],27),(fromList [2,3,1],78),(fromList [3,2,1],438),(fromList [3,1,2],1836)]

solve1 :: Seq Word -> Word
solve1 = (!! 2020) . generate

solve2 :: Seq Word -> Maybe Word
solve2 = solve 30000000

solve :: Word -> Seq Word -> Maybe Word
solve ix =
  loadPrefix
    .> fmap
      ( iterate go
          .> dropWhile ((< ix) . fst . fst)
          .> head
          .> snd
      )
 where
  (.>) = flip (.)
  go :: ((Word, Map Word Word), Word) -> ((Word, Map Word Word), Word)
  go ((index, memory), latest) =
    let nextState = (succ index, Map.insert latest index memory)
     in case Map.lookup latest memory of
          Nothing -> (nextState, 0)
          Just previousIndex -> (nextState, index - previousIndex)

generate :: Seq Word -> [Word]
generate ws =
  Foldable.toList ws ++ maybe [] (fmap snd . iterate go) (loadPrefix ws)
 where
  go :: ((Word, Map Word Word), Word) -> ((Word, Map Word Word), Word)
  go ((index, memory), latest) =
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
  goPrefix ((index, memory), latest) (x :<| xs) =
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

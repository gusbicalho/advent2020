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
import Data.Coerce (coerce)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Maybe qualified as Maybe
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

solve1 :: [a] -> Int
solve1 = length

solve2 :: [a] -> Int
solve2 = length

newtype Ingredient = Ingredient String
  deriving stock (Eq, Ord, Show)
newtype Allergen = Allergen String
  deriving stock (Eq, Ord, Show)
data Recipe = Recipe [Ingredient] (Set Allergen)
  deriving stock (Eq, Ord, Show)

parseRecipe :: String -> Maybe Recipe
parseRecipe cs = do
  let (front, back) = span (/= '(') cs
  allergensPart <- List.stripPrefix "(contains" back
  let allergens = fmap (filter Char.isAlpha) . words $ allergensPart
      ingredients = fmap (filter Char.isAlpha) . words $ front
  pure $ Recipe (coerce ingredients) (Set.fromList $ coerce allergens)

example :: [Recipe]
example =
  Maybe.mapMaybe
    parseRecipe
    [ "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
    , "trh fvjkl sbzzf mxmxvkd (contains dairy)"
    , "sqjhc fvjkl (contains soy)"
    , "sqjhc mxmxvkd sbzzf (contains fish)"
    ]

input :: IO [Recipe]
input = Foldable.toList <$> Advent2020.Input.loadInput parseRecipe "day21/input"

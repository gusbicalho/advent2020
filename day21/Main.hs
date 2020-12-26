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
import Control.Arrow (Arrow ((&&&)))
import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char qualified as Char
import Data.Coerce (coerce)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple qualified as Tuple

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show $ solve1 example
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show $ solve2 example
  putStrLn . ("for real: " <>) . show . solve2 =<< input

solve1 :: [Recipe] -> Word
solve1 recipes =
  List.genericLength @Word $ filter (`elem` definitelyNonAllergenicIngredients) ingredients
 where
  definitelyNonAllergenicIngredients =
    filter (`Map.notMember` i2a)
      . Foldable.toList
      . Set.fromList
      $ ingredients
  ingredients = allIngredients recipes
  (_, i2a) = investigateAllergens recipes

solve2 :: [Recipe] -> Maybe String
solve2 recipes = Maybe.listToMaybe $ toCanonicalList <$> go obviousGuesses
 where
  toCanonicalList =
    List.intercalate ","
      . coerce
      . fmap fst
      . List.sortOn snd
      . Map.toList
  go :: (Map Ingredient Allergen, Map Allergen Ingredient) -> [Map Ingredient Allergen]
  go previousGuesses =
    let newGuesses = do
          ingredient <- possiblyAllergenicIngredients
          guard $ ingredient `Map.notMember` fst previousGuesses
          allergen <- maybe [] Foldable.toList $ Map.lookup ingredient i2a
          guard $ allergen `Map.notMember` snd previousGuesses
          guard $ isPossible ingredient allergen
          pure (ingredient, allergen)
     in case newGuesses of
          [] -> do
            guard $ all (`Map.member` snd previousGuesses) allergens
            pure $ fst previousGuesses
          _ -> do
            guess <- newGuesses
            go $ addGuess guess previousGuesses
  isPossible ingredient allergen = maybe False (ingredient `elem`) $ Map.lookup allergen a2i
  obviousGuesses =
    Foldable.foldl' (flip addGuess) (Map.empty, Map.empty)
      $ Maybe.mapMaybe (\(a, i :| is) -> if null is then Just (i,a) else Nothing) (Map.toList a2i)
        ++ Maybe.mapMaybe (\(i, a :| as) -> if null as then Just (i,a) else Nothing) (Map.toList i2a)
  addGuess (ingredient, allergen) =
    bimap (Map.insert ingredient allergen) (Map.insert allergen ingredient)
  possiblyAllergenicIngredients = Map.keys i2a
  allergens = Foldable.toList . allAllergens $ recipes
  (a2i, i2a) = investigateAllergens recipes

allAllergens :: Foldable t => t Recipe -> Set Allergen
allAllergens = Foldable.foldMap' getAllergens

allIngredients :: Foldable t => t Recipe -> [Ingredient]
allIngredients = Foldable.foldMap' getIngredients

investigateAllergens :: [Recipe] -> (Map Allergen (NonEmpty Ingredient), Map Ingredient (NonEmpty Allergen))
investigateAllergens = (id &&& invert) . possibleAllergenSources
 where
  invert :: Map Allergen (NonEmpty Ingredient) -> Map Ingredient (NonEmpty Allergen)
  invert a2i = Foldable.foldl' absorb Map.empty . fmap Tuple.swap . explode $ a2i
  explode :: Map a (NonEmpty b) -> [(a, b)]
  explode m = do
    (a, bs) <- Map.toList m
    b <- Foldable.toList bs
    pure (a, b)
  absorb :: Ord b => Map b (NonEmpty a) -> (b, a) -> Map b (NonEmpty a)
  absorb m (k, v) = Map.insertWith (<>) k (pure v) m

possibleAllergenSources :: [Recipe] -> Map Allergen (NonEmpty Ingredient)
possibleAllergenSources recipes =
  Map.fromList
    . Maybe.mapMaybe (\a -> (a,) <$> NonEmpty.nonEmpty (possibleSources a))
    . Foldable.toList
    . allAllergens
    $ recipes
 where
  possibleSources :: Allergen -> [Ingredient]
  possibleSources a =
    case fmap (Set.fromList . getIngredients) . filter (containAllergen a) $ recipes of
      [] -> []
      (is : iss) -> Foldable.toList $ Foldable.foldl' Set.intersection is iss
  containAllergen :: Allergen -> Recipe -> Bool
  containAllergen a (Recipe _ as) = a `Set.member` as

newtype Ingredient = Ingredient String
  deriving stock (Eq, Ord, Show)
newtype Allergen = Allergen String
  deriving stock (Eq, Ord, Show)
data Recipe = Recipe
  { getIngredients :: [Ingredient]
  , getAllergens :: Set Allergen
  }
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

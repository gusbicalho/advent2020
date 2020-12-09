{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Advent2020.Day07 (day07_01, day07_02) where

import Advent2020.Input qualified
import Data.Foldable qualified as Foldable
import Data.Functor (($>))
import Data.Map.Merge.Strict qualified as Map.Merge
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import GHC.Natural (Natural)
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Char qualified as Parsec.Char
import Text.Megaparsec.Char.Lexer qualified as Parsec.Char.Lexer

day07_01 :: IO Int
day07_01 = solve01 <$> input

solve01 :: Seq Rule -> Int
solve01 rules = Set.size $ closeTransitively allowedByRules (Set.singleton $ BagType "shiny gold")
 where
  allowedByRules :: BagType -> Set BagType
  allowedByRules bt = Set.fromList . fmap bagType . filter (allows 1 bt) . Foldable.toList $ rules

allows :: Natural -> BagType -> Rule -> Bool
allows n bt (Rule _ contents) = case Map.lookup bt contents of
  Nothing -> False
  Just maxBags -> n <= maxBags

-- >>> solve01 example
-- 4

-- >>> day07_01
-- 185

day07_02 :: IO Int
day07_02 = solve02 <$> input

solve02 :: Seq Rule -> Int
solve02 _ = 1

-- >>> day07_02
-- 3288

newtype BagType = BagType String
  deriving stock (Eq, Show, Ord)

newtype MustContainRelations = MustContainRelations {unMustContainIndex :: Map BagType (Map BagType Natural)}

data Rule = Rule
  { bagType :: BagType
  , contents :: Map BagType Natural
  }
  deriving stock (Eq, Show)

closeTransitively :: Ord a => (a -> Set a) -> Set a -> Set a
closeTransitively prop = Foldable.fold . drop 1 . transitively prop

transitively :: Ord a => (a -> Set a) -> Set a -> [Set a]
transitively prop = go Set.empty
 where
  go previouslyKnown newlyGenerated =
    let diff = Set.difference newlyGenerated previouslyKnown
     in if Set.empty == diff
          then []
          else
            diff :
            go
              (Set.union previouslyKnown diff)
              (Foldable.foldMap prop diff)

mustContainRelations :: Seq Rule -> MustContainRelations
mustContainRelations = MustContainRelations . Foldable.foldl' addRule Map.empty
 where
  addRule :: Map BagType (Map BagType Natural) -> Rule -> Map BagType (Map BagType Natural)
  addRule ruleMap (Rule bt contents) = Map.insertWith mergeRule bt contents ruleMap
  mergeRule :: Map BagType Natural -> Map BagType Natural -> Map BagType Natural
  mergeRule =
    Map.Merge.merge
      Map.Merge.preserveMissing'
      Map.Merge.preserveMissing'
      (Map.Merge.zipWithMaybeMatched (\_ n1 n2 -> Just (n1 + n2)))

ruleParser :: Parsec.Parsec Void String Rule
ruleParser = Rule <$> bagType <*> contents
 where
  bagType :: Parsec.Parsec Void String BagType
  bagType = BagType <$> Parsec.manyTill Parsec.anySingle (Parsec.Char.string " bags contain")
  contents :: Parsec.Parsec Void String (Map BagType Natural)
  contents =
    Foldable.asum
      [ Parsec.Char.string " no other bags." $> Map.empty
      , fmap Foldable.fold . Parsec.many $ do
          Parsec.Char.space1
          count :: Natural <- Parsec.Char.Lexer.decimal
          Parsec.Char.space1
          innerBagType <-
            BagType
              <$> Parsec.manyTill
                Parsec.anySingle
                ( Parsec.Char.string " bag"
                    *> Parsec.optional (Parsec.Char.char 's')
                    *> Parsec.oneOf ",."
                )
          pure $ Map.singleton innerBagType count
      ]

example :: Seq Rule
example =
  Seq.fromList . Maybe.fromJust . traverse (Parsec.parseMaybe ruleParser) $
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
    , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
    , "bright white bags contain 1 shiny gold bag."
    , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
    , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
    , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
    , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
    , "faded blue bags contain no other bags."
    , "dotted black bags contain no other bags."
    ]

-- >>> example

input :: IO (Seq Rule)
input = Advent2020.Input.loadInput (Parsec.parseMaybe ruleParser) "resources/day07/input"

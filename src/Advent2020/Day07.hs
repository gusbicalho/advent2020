{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE TypeFamilies #-}
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
day07_01 = solve01 . ruleIndex <$> input

solve01 :: RuleIndex -> Int
solve01 index =
  Set.size
    . Foldable.fold
    . drop 1 -- this will always be the initial set, the shiny gold bag
    . transitively allowedByRules
    $ Set.singleton initialBag
 where
  initialBag = BagType "shiny gold"
  allowedByRules :: BagType -> Set BagType
  allowedByRules bt = Map.keysSet . Map.filter (allows 1 bt) $ index
  allows :: Natural -> BagType -> Map BagType Natural -> Bool
  allows n bt contents = case Map.lookup bt contents of
    Nothing -> False
    Just maxBags -> n <= maxBags

-- >>> solve01 . ruleIndex $ example
-- 4

-- >>> day07_01
-- 185

day07_02 :: IO Natural
day07_02 = solve02 . ruleIndex <$> input

solve02 :: RuleIndex -> Natural
solve02 index = collect (BagType "shiny gold")
 where
  collect :: BagType -> Natural
  collect key = case Map.lookup key index of
    Nothing -> 0
    Just contents ->
      sum
        . fmap (\(k, n) -> let !subtotal = collect k in n + n * subtotal)
        . Map.toList
        $ contents

-- >>> solve02 . ruleIndex $ example
-- 32

-- >>> solve02 . ruleIndex $ example2
-- 126

-- >>> day07_02
-- 89084

newtype BagType = BagType String
  deriving stock (Eq, Show, Ord)

type RuleIndex = Map BagType (Map BagType Natural)

data Rule = Rule
  { bagType :: BagType
  , contents :: Map BagType Natural
  }
  deriving stock (Eq, Show)

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

ruleIndex :: Seq Rule -> RuleIndex
ruleIndex = Foldable.foldl' addRule Map.empty
 where
  addRule ruleMap (Rule bt contents) =
    Map.insertWith (mergeMapsWith (+)) bt contents ruleMap

mergeMapsWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
mergeMapsWith mergeVals =
  Map.Merge.merge
    Map.Merge.preserveMissing'
    Map.Merge.preserveMissing'
    (Map.Merge.zipWithMaybeMatched (\_ n1 n2 -> Just (mergeVals n1 n2)))

ruleParser :: Parsec.Parsec Void String Rule
ruleParser =
  Rule <$> bagTypeEndingWith (Parsec.Char.string " bags contain") <*> contents
 where
  bagTypeEndingWith :: Parsec.Parsec Void String a -> Parsec.Parsec Void String BagType
  bagTypeEndingWith suffix = BagType <$> Parsec.manyTill Parsec.anySingle suffix
  contents :: Parsec.Parsec Void String (Map BagType Natural)
  contents =
    Foldable.asum
      [ Parsec.Char.string " no other bags." $> Map.empty
      , foldMany $ do
          Parsec.Char.space1
          count :: Natural <- Parsec.Char.Lexer.decimal
          Parsec.Char.space1
          innerBagType <-
            bagTypeEndingWith
              ( Parsec.Char.string " bag"
                  *> Parsec.optional (Parsec.Char.char 's')
                  *> Parsec.oneOf ",."
              )
          pure $ Map.singleton innerBagType count
      ]
  foldMany :: Monoid m => Parsec.ParsecT Void String a m -> Parsec.ParsecT Void String a m
  foldMany = fmap Foldable.fold . Parsec.many

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

example2 :: Seq Rule
example2 =
  Seq.fromList . Maybe.fromJust . traverse (Parsec.parseMaybe ruleParser) $
    [ "shiny gold bags contain 2 dark red bags."
    , "dark red bags contain 2 dark orange bags."
    , "dark orange bags contain 2 dark yellow bags."
    , "dark yellow bags contain 2 dark green bags."
    , "dark green bags contain 2 dark blue bags."
    , "dark blue bags contain 2 dark violet bags."
    , "dark violet bags contain no other bags."
    ]

input :: IO (Seq Rule)
input = Advent2020.Input.loadInput (Parsec.parseMaybe ruleParser) "resources/day07/input"

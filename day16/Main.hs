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
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq, (<|))
import Data.Sequence qualified as Seq
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P.Char
import Text.Megaparsec.Char.Lexer qualified as P.Char.Lexer

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show $ solve1 example
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  putStrLn . ("example2: " <>) . show $ solve2 example2
  putStrLn . ("for real: " <>) . show . solve2 =<< input

-- >>> solve1 example
-- 71

solve1 :: Input -> Word
solve1 Input{rules, nearbyTickets} = errorRate
 where
  errorRate = Foldable.sum invalidValues
  invalidValues = Foldable.foldMap invalidValuesForTicket nearbyTickets
  invalidValuesForTicket (Ticket values) = Seq.filter (isInvalidValue rules) values

-- >>> solve2 example
-- Just 1

solve2 :: Input -> Maybe Word
solve2 Input{rules, myTicket, nearbyTickets} =
  Maybe.listToMaybe $
    acceptableFieldMappings cleanTickets rules
      <&> \fieldOrder ->
        let myTicketFields = toFieldMap fieldOrder myTicket
         in myTicketFields
              & Map.toAscList
              & filter (("departure" `List.isPrefixOf`) . fst)
              <&> snd
              & product
 where
  cleanTickets = Seq.filter (not . hasInvalidValues) . (myTicket <|) $ nearbyTickets
  hasInvalidValues (Ticket values) = Foldable.any (isInvalidValue rules) values
  toFieldMap fieldOrder (Ticket values) = flip Map.mapMaybe fieldOrder $ \ix ->
    values Seq.!? ix

acceptableFieldMappings :: Seq Ticket -> Rules -> [Map String Int]
acceptableFieldMappings tickets =
  go Map.empty IntSet.empty
    . List.sortOn (IntSet.size . snd)
    . Map.toAscList
    . Map.map acceptableIndexes
 where
  fieldCount = maybe 0 (Seq.length . unTicket) $ tickets Seq.!? 0
  acceptableIndexes :: Rule -> IntSet
  acceptableIndexes ranges = IntSet.fromList $ filter (rangesAcceptAll ranges . valuesAt) [0 .. fieldCount -1]
  go :: Map String Int -> IntSet -> [(String, IntSet)] -> [Map String Int]
  go acc _ [] = [acc]
  go acc usedIndexes ((field, possibleIndexes) : morePairs) = do
    index <- IntSet.toList $ IntSet.difference possibleIndexes usedIndexes
    go (Map.insert field index acc) (IntSet.insert index usedIndexes) morePairs
  valuesAt :: Int -> [Word]
  valuesAt ix = Maybe.mapMaybe ((Seq.!? ix) . unTicket) . Foldable.toList $ tickets

data Range = Range {fromInclusive :: Word, toInclusive :: Word}
  deriving stock (Eq, Ord, Show)
newtype Ticket = Ticket {unTicket :: Seq Word}
  deriving stock (Eq, Ord, Show)
type Rules = Map String Rule
type Rule = Seq Range
data Input = Input
  { rules :: Rules
  , myTicket :: Ticket
  , nearbyTickets :: Seq Ticket
  }
  deriving stock (Eq, Ord, Show)

isInvalidValue :: Rules -> Word -> Bool
isInvalidValue rules = null . fieldsAcceptingAll rules . pure

fieldsAcceptingAll :: Rules -> [Word] -> [String]
fieldsAcceptingAll rules values =
  rules & Map.toAscList & filter (flip rangesAcceptAll values . snd) & fmap fst

rangesAcceptAll :: (Foldable t1, Foldable t2) => t2 Range -> t1 Word -> Bool
rangesAcceptAll ranges = Foldable.all (ranges `rangesAccept`)

rangesAccept :: Foldable t => t Range -> Word -> Bool
rangesAccept ranges w = Foldable.any (`rangeAccepts` w) ranges

rangeAccepts :: Range -> Word -> Bool
rangeAccepts Range{fromInclusive, toInclusive} w =
  fromInclusive <= w && w <= toInclusive

type Parser a = P.Parsec Void String a

parseInput :: Foldable t => t String -> Maybe Input
parseInput ls = P.parseMaybe inputP (Foldable.foldMap (<> "\n") ls)

inputP :: Parser Input
inputP = do
  rules <- rulesP
  let fieldCount = Map.size rules
  matchLine "your ticket:"
  myTicket <- ticketP fieldCount
  blankLine
  matchLine "nearby tickets:"
  nearbyTickets <- Seq.fromList <$> P.many (ticketP fieldCount)
  pure Input{rules, myTicket, nearbyTickets}
 where
  rulesP :: Parser Rules
  rulesP = Map.fromList <$> P.manyTill ruleP blankLine
  ruleP :: Parser (String, Rule)
  ruleP = do
    fieldName <- P.manyTill P.anySingle (P.Char.string ": ")
    ranges <- Seq.fromList <$> P.manyTill (rangeP <* P.optional (P.Char.string " or ")) P.Char.eol
    pure (fieldName, ranges)
  rangeP :: Parser Range
  rangeP = do
    lower <- P.Char.Lexer.decimal
    P.Char.char '-'
    upper <- P.Char.Lexer.decimal
    when (lower > upper) $
      fail $ "Range with lower = " <> show lower <> "  > upper = " <> show upper
    pure $ Range lower upper
  ticketP :: Int -> Parser Ticket
  ticketP fieldCount = do
    fields <- P.count fieldCount (P.Char.Lexer.decimal <* P.optional (P.Char.char ','))
    P.Char.eol
    pure . Ticket . Seq.fromList $ fields
  matchLine :: String -> Parser ()
  matchLine s = P.Char.string s *> P.Char.eol $> ()
  blankLine :: Parser ()
  blankLine = P.label "blank line" $ matchLine ""

example :: Input
example =
  Maybe.fromJust . parseInput . Seq.fromList $
    [ "class: 1-3 or 5-7"
    , "row: 6-11 or 33-44"
    , "seat: 13-40 or 45-50"
    , ""
    , "your ticket:"
    , "7,1,14"
    , ""
    , "nearby tickets:"
    , "7,3,47"
    , "40,4,50"
    , "55,2,20"
    , "38,6,12"
    ]

example2 :: Input
example2 =
  Maybe.fromJust . parseInput . Seq.fromList $
    [ "class: 0-1 or 4-19"
    , "row: 0-5 or 8-19"
    , "seat: 0-13 or 16-19"
    , ""
    , "your ticket:"
    , "11,12,13"
    , ""
    , "nearby tickets:"
    , "3,9,18"
    , "15,1,5"
    , "5,14,9"
    ]

input :: IO Input
input = do
  ls <- Advent2020.Input.loadInput Just "day16/input"
  case parseInput ls of
    Nothing -> throwIO $ userError "Bad input"
    Just i -> pure i

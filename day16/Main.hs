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
import Data.Functor (($>), (<&>))
import Data.Map.Strict (Map)
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P.Char
import Text.Megaparsec.Char.Lexer qualified as P.Char.Lexer
import qualified Data.Foldable as Foldable
import Data.Void (Void)
import qualified Data.Map.Strict as Map
import Control.Monad (when)
import qualified Data.Set as Set


main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show $ solve1 example
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show $ solve2 example
  putStrLn . ("for real: " <>) . show . solve2 =<< input

solve1 :: Input -> Int
solve1 _ = 0

solve2 :: Input -> Int
solve2 _ = 0

data Range = Range {fromInclusive :: Word, toInclusive :: Word}
  deriving stock (Eq, Ord, Show)
newtype Ticket = Ticket (Seq Word)
  deriving stock (Eq, Ord, Show)
type Rules = Map String (Seq Range)
data Input = Input
  { rules :: Rules
  , myTicket :: Ticket
  , nearbyTickets :: Seq Ticket
  }
  deriving stock (Eq, Ord, Show)

type Parser a = P.Parsec Void String a
parseInput :: Seq String -> Maybe Input
parseInput ls = P.parseMaybe inputP (Foldable.foldMap (<>"\n") ls)
 where
  inputP :: Parser Input
  inputP = do
    rules <- rulesP
    let fieldCount = Map.size rules
    blankLine
    matchLine "your ticket:"
    myTicket <- ticketP fieldCount
    blankLine
    matchLine "nearby tickets:"
    nearbyTickets <- Seq.fromList <$> P.many (ticketP fieldCount)
    pure Input {rules, myTicket, nearbyTickets}
  rulesP :: Parser Rules
  rulesP = Map.fromList <$> P.manyTill ruleP blankLine
  ruleP :: Parser (String, Seq Range)
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
      fail $ "Range with lower = " <> show lower <>"  > upper = " <> show upper
    pure $ Range lower upper
  ticketP :: Int -> Parser Ticket
  ticketP fieldCount = do
    fields <- P.count fieldCount (P.Char.Lexer.decimal <* P.optional (P.Char.char ','))
    P.Char.eol
    pure . Ticket . Seq.fromList $ fields
  matchLine :: String -> Parser ()
  matchLine s = P.Char.string s *> P.Char.eol $> ()
  blankLine :: Parser ()
  blankLine = matchLine ""

-- >>> example

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

input :: IO Input
input = do
  ls <- Advent2020.Input.loadInput Just "day16/input"
  case parseInput ls of
    Nothing -> throwIO $ userError "Bad input"
    Just i -> pure i

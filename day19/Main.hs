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
import Control.Monad (void)
import Data.Foldable qualified as Foldable
import Data.Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P.Char
import Text.Megaparsec.Char.Lexer qualified as P.Lexer

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show $ solve1 example
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show $ solve2 example
  putStrLn . ("for real: " <>) . show . solve2 =<< input

solve1 :: Seq a -> Int
solve1 = Seq.length

solve2 :: Seq a -> Int
solve2 = Seq.length

data RuleDescription
  = RuleDescLit String
  | RuleDescRef Word
  | RuleDescConcat RuleDescription RuleDescription
  | RuleDescAlt RuleDescription RuleDescription
  deriving (Eq, Ord, Show)

data Rule
  = RuleLit String
  | RuleConcat Rule Rule
  | RuleAlt Rule Rule
  deriving (Eq, Ord, Show)

newtype Rules = Rules {unRules :: Map Word Rule}
  deriving (Eq, Ord, Show)

type Parser a = P.Parsec Void String a

-- >>> P.parseTest ruleP "\"a\""

ruleP :: Parser RuleDescription
ruleP = (litP <|> altP) <* (P.eof <|> void P.Char.eol)
 where
  litP :: Parser RuleDescription
  litP =
    fmap RuleDescLit
      . P.between (P.Char.char '"') (P.Char.char '"')
      $ P.many P.anySingle
  spaces = P.many P.Char.space
  altSeparator = P.between spaces spaces (P.Char.char '|')
  altP :: Parser RuleDescription
  altP = do
    concats <- P.sepBy1 concatP altSeparator
    pure . foldr1 RuleDescConcat $ concats
  concatP :: Parser RuleDescription
  concatP = do
    refs <- P.sepBy1 (RuleDescRef <$> ruleRefP) (P.some P.Char.space)
    pure . foldr1 RuleDescConcat $ refs
  ruleRefP = P.Lexer.decimal

rulesP :: Parser Rules
rulesP = Rules <$> (fixRules =<< rulesMapP)
 where
  rulesMapP :: Parser (Map Word RuleDescription)
  rulesMapP = undefined
  fixRules :: Map Word RuleDescription -> Parser (Map Word Rule)
  fixRules rulesMap = undefined

-- parseInput :: Foldable t => t String -> ([String], Rules)

example :: Seq String
example =
  Seq.fromList
    [ "0: 4 1 5"
    , "1: 2 3 | 3 2"
    , "2: 4 4 | 5 5"
    , "3: 4 5 | 5 4"
    , "4: \"a\""
    , "5: \"b\""
    , ""
    , "ababbb"
    , "bababa"
    , "abbbab"
    , "aaabbb"
    , "aaaabbb"
    ]

input :: IO (Seq String)
input = Advent2020.Input.loadInput Just "day19/input"

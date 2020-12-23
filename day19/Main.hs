{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Advent2020.Input qualified
import Control.Monad (void, (>=>))
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
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
  putStrLn . ("example: " <>) . show . solve $ example
  putStrLn . ("for real: " <>) . show . solve =<< input
  putStrLn "Part 2"
  putStrLn . ("for real: " <>) . show . solve =<< input2

solve :: Foldable t => t String -> Int
solve inputLines =
  inputLines
    & Foldable.toList
    & unlines
    & P.parseMaybe inputP
    & \case
      Nothing -> 0
      Just (rules, messages) ->
        messages
          & Foldable.toList
          & filter (ruleAccepted . runRuleDescription rules 0)
          & length

-- a rule is valid if there is at least one possibility
-- where it consumed the full input
ruleAccepted :: [String] -> Bool
ruleAccepted = any null

runRuleDescription :: Map Word RuleDescription -> Word -> String -> [String]
runRuleDescription rules = goLookup
 where
  goLookup :: Word -> String -> [String]
  goLookup ix = case Map.lookup ix rules of
    Nothing -> const []
    Just ruleDesc -> go ruleDesc
  go (RuleDescRef w) = goLookup w
  go (RuleDescLit lit) = Maybe.maybeToList . List.stripPrefix lit
  go (RuleDescConcat ruleA ruleB) = go ruleA >=> go ruleB
  go (RuleDescAlt ruleA ruleB) = \s -> go ruleA s <|> go ruleB s

data RuleDescription
  = RuleDescLit String
  | RuleDescRef Word
  | RuleDescConcat RuleDescription RuleDescription
  | RuleDescAlt RuleDescription RuleDescription
  deriving stock (Eq, Ord, Show)

type Parser a = P.Parsec Void String a

inputP :: Parser (Map Word RuleDescription, Seq String)
inputP = do
  ruleDescriptions <- ruleDescriptionsP
  _ <- P.Char.eol -- blank line
  inputLines <- linesP
  pure (ruleDescriptions, inputLines)

linesP :: Parser (Seq String)
linesP = Seq.fromList . lines <$> P.many P.anySingle

ruleDescriptionsP :: Parser (Map Word RuleDescription)
ruleDescriptionsP = Map.fromList <$> P.many rulePairP
 where
  rulePairP :: Parser (Word, RuleDescription)
  rulePairP = do
    ix <- P.Lexer.decimal :: Parser Word
    P.Char.string ": "
    ruleDesc <- litP <|> altP
    P.eof <|> void P.Char.eol
    pure (ix, ruleDesc)
  litP :: Parser RuleDescription
  litP = RuleDescLit <$> quoted (P.many (P.anySingleBut '"'))
  altP :: Parser RuleDescription
  altP = foldr1 RuleDescAlt <$> leftAssociativeOp concatP (P.Char.char '|')
  concatP :: Parser RuleDescription
  concatP = foldr1 RuleDescConcat <$> leftAssociativeOp ruleRefP (pure ())
  ruleRefP :: Parser RuleDescription
  ruleRefP = RuleDescRef <$> P.Lexer.decimal

quoted :: Ord e => P.Parsec e String a -> P.Parsec e String a
quoted = P.between (P.Char.char '"') (P.Char.char '"')

leftAssociativeOp :: Parser a -> Parser sep -> Parser (NonEmpty a)
leftAssociativeOp termP separatorP = go
 where
  go = do
    leftTerm <- termP
    P.Char.hspace
    maybeMore <- P.optional $ do
      separatorP
      P.Char.hspace
      go
    case maybeMore of
      Nothing -> pure $ leftTerm :| []
      Just more -> pure $ leftTerm NonEmpty.<| more

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

input2 :: IO (Seq String)
input2 = Advent2020.Input.loadInput Just "day19/input2"

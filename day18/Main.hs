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
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Text.Read qualified as Read
import Prelude hiding (exp)

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show $ solve1 example
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show $ solve2 example
  putStrLn . ("for real: " <>) . show . solve2 =<< input

-- >>> solve1 example
-- 26406

solve1 :: Seq (Seq Lexeme) -> Word
solve1 = Foldable.sum . fmap (either (const 0) id . parseExp1 @Word)

parseExp1 :: forall e t. (Foldable t, Exp e) => t Lexeme -> Either String e
parseExp1 = fmap snd . exp . Foldable.toList
 where
  -- ls, moreLs :: [Lexeme]
  exp :: [Lexeme] -> Either String ([Lexeme], e)
  exp ls = do
    (moreLs, e) <- term ls
    binaryOpTail e moreLs
  term :: [Lexeme] -> Either String ([Lexeme], e)
  term (Number i : ls) = Right (ls, number i)
  term (LeftParen : ls) =
    exp ls >>= \case
      (RightParen : moreLs, e) -> Right (moreLs, e)
      (moreLs, _) -> Left $ "Expected RightParen, found " <> show moreLs
  term ls = Left $ "Expected LeftParen or Number, found " <> show ls
  binaryOpTail :: e -> [Lexeme] -> Either String ([Lexeme], e)
  binaryOpTail leftTerm (Operator op : ls) = do
    (moreLs, rightTerm) <- term ls
    binaryOpTail (evalOp op leftTerm rightTerm) moreLs
  binaryOpTail leftTerm ls@(RightParen : _) = Right (ls, leftTerm)
  binaryOpTail leftTerm [] = Right ([], leftTerm)
  binaryOpTail _ ls = Left $ "Expected Operator, found " <> show ls

solve2 :: Seq (Seq Lexeme) -> Word
solve2 = Foldable.sum . fmap (either (const 0) id . parseExp2 @Word)

type ExpParser e = [Lexeme] -> Either String ([Lexeme], e)

parseExp2 :: forall e t. (Foldable t, Exp e) => t Lexeme -> Either String e
parseExp2 = fmap snd . exp . Foldable.toList
 where
  -- ls, moreLs :: [Lexeme]
  exp = binaryRightAssociativeOps [Times, Plus] term
  binaryRightAssociativeOps :: [Operator] -> ExpParser e -> ExpParser e
  binaryRightAssociativeOps [] termParser ls = termParser ls
  binaryRightAssociativeOps ops@(op : higherPrecedenceOps) termParser ls =
    binaryRightAssociativeOps higherPrecedenceOps termParser ls >>= \case
      (Operator foundOp : moreLs, leftTerm) | foundOp == op ->
        (evalOp op leftTerm <$>) <$> binaryRightAssociativeOps ops termParser moreLs
      result -> pure result
  term :: ExpParser e
  term (Number i : ls) = Right (ls, number i)
  term (LeftParen : ls) =
    exp ls >>= \case
      (RightParen : moreLs, e) -> Right (moreLs, e)
      (moreLs, _) -> Left $ "Expected RightParen, found " <> show moreLs
  term ls = Left $ "Expected LeftParen or Number, found " <> show ls

class Exp e where
  number :: Word -> e
  times :: e -> e -> e
  plus :: e -> e -> e

evalOp :: Exp e => Operator -> e -> e -> e
evalOp Plus = plus
evalOp Times = times

instance Exp Word where
  number = id
  times = (*)
  plus = (+)

instance Exp AST where
  number = ANum
  times = ABinOp Times
  plus = ABinOp Plus

data AST
  = ANum Word
  | ABinOp Operator AST AST
  deriving stock (Eq, Ord, Show)

data Operator = Plus | Times
  deriving stock (Eq, Ord, Show)
data Lexeme
  = LeftParen
  | RightParen
  | Operator Operator
  | Number Word
  deriving stock (Eq, Ord, Show)

parseLexemes :: String -> Maybe (Seq Lexeme)
parseLexemes = fmap Seq.fromList . go
 where
  go :: String -> Maybe [Lexeme]
  go [] = Just []
  go (' ' : cs) = go cs
  go ('*' : cs) = (Operator Times :) <$> go cs
  go ('+' : cs) = (Operator Plus :) <$> go cs
  go ('(' : cs) = (LeftParen :) <$> go cs
  go (')' : cs) = (RightParen :) <$> go cs
  go (span Char.isDigit -> (digits, cs)) = case Read.readMaybe @Word digits of
    Just i -> (Number i :) <$> go cs
    Nothing -> Nothing

example :: Seq (Seq Lexeme)
example =
  Seq.fromList
    . Maybe.mapMaybe parseLexemes
    $ [ "1 + 2 * 3 + 4 * 5 + 6"
      , "2 * 3 + (4 * 5)"
      , "5 + (8 * 3 + 9 + 3 * 4 * 3)"
      , "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
      , "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
      ]

input :: IO (Seq (Seq Lexeme))
input = Advent2020.Input.loadInput parseLexemes "day18/input"

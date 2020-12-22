{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Advent2020.Input qualified
import Control.Arrow (Arrow ((&&&)))
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Text.Read qualified as Read
import Prelude hiding (exp)

main :: IO ()
main = do
  putStrLn "Part 1"
  case (id &&& parseExp1 @AST &&& parseExp1 @Word) $ example of
    (lexemes, (Right ast, Right result)) -> do
      putStrLn "example:"
      putStrLn $ "> lexemes: " <> showLexemes lexemes
      putStrLn $ "> AST:     " <> showAST ast
      putStrLn $ "> result:  " <> show result
    _ -> pure ()
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  case (id &&& parseExp2 @AST &&& parseExp2 @Word) $ example of
    (lexemes, (Right ast, Right result)) -> do
      putStrLn "example:"
      putStrLn $ "> lexemes: " <> showLexemes lexemes
      putStrLn $ "> AST:     " <> showAST ast
      putStrLn $ "> result:  " <> show result
    _ -> pure ()
  putStrLn . ("for real: " <>) . show . solve2 =<< input

-- >>> solve1 [example]
-- 13632

solve1 :: Foldable t => t (Seq Lexeme) -> Word
solve1 = Foldable.sum . fmap (either (const 0) id . parseExp1 @Word) . Foldable.toList

parseExp1 :: forall e t. (Foldable t, Exp e) => t Lexeme -> Either String e
parseExp1 = fmap snd . exp . Foldable.toList
 where
  exp :: ExpParser e
  exp ls = do
    (moreLs, e) <- term ls
    binaryOpTail e moreLs
  term :: ExpParser e
  term (Number i : ls) = Right (ls, number i)
  term (LeftParen : ls) =
    exp ls >>= \case
      (RightParen : moreLs, e) -> Right (moreLs, e)
      (moreLs, _) -> Left $ "Expected RightParen, found " <> show moreLs
  term ls = Left $ "Expected LeftParen or Number, found " <> show ls
  binaryOpTail :: e -> ExpParser e
  binaryOpTail leftTerm (Operator op : ls) = do
    (moreLs, rightTerm) <- term ls
    binaryOpTail (evalOp op leftTerm rightTerm) moreLs
  binaryOpTail leftTerm ls@(RightParen : _) = Right (ls, leftTerm)
  binaryOpTail leftTerm [] = Right ([], leftTerm)
  binaryOpTail _ ls = Left $ "Expected Operator, found " <> show ls

-- >>> solve2 [example]
-- 23340

solve2 :: Foldable t => t (Seq Lexeme) -> Word
solve2 = Foldable.sum . fmap (either (const 0) id . parseExp2 @Word) . Foldable.toList

parseExp2 :: forall e t. (Foldable t, Exp e) => t Lexeme -> Either String e
parseExp2 = fmap snd . exp . Foldable.toList
 where
  exp = binaryRightAssociativeOps [Times, Plus] term
  binaryRightAssociativeOps :: [Operator] -> ExpParser e -> ExpParser e
  binaryRightAssociativeOps [] termParser ls = termParser ls
  binaryRightAssociativeOps ops@(op : higherPrecedenceOps) termParser ls =
    binaryRightAssociativeOps higherPrecedenceOps termParser ls >>= \case
      (Operator foundOp : moreLs, leftTerm)
        | foundOp == op ->
          (evalOp op leftTerm <$>) <$> binaryRightAssociativeOps ops termParser moreLs
      result -> pure result
  term :: ExpParser e
  term (Number i : ls) = Right (ls, number i)
  term (LeftParen : ls) =
    exp ls >>= \case
      (RightParen : moreLs, e) -> Right (moreLs, e)
      (moreLs, _) -> Left $ "Expected RightParen, found " <> show moreLs
  term ls = Left $ "Expected LeftParen or Number, found " <> show ls

type ExpParser e = [Lexeme] -> Either String ([Lexeme], e)

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

showAST :: AST -> [Char]
showAST (ANum w) = show w
showAST (ABinOp op a b) =
  "("
    <> showAST a
    <> " "
    <> showOperator op
    <> " "
    <> showAST b
    <> ")"

data Operator = Plus | Times
  deriving stock (Eq, Ord, Show)
data Lexeme
  = LeftParen
  | RightParen
  | Operator Operator
  | Number Word
  deriving stock (Eq, Ord, Show)

showOperator :: Operator -> [Char]
showOperator Plus = "+"
showOperator Times = "*"

showLexemes :: Foldable t => t Lexeme -> String
showLexemes = unwords . fmap showLexeme . Foldable.toList
 where
  showLexeme LeftParen = "("
  showLexeme RightParen = ")"
  showLexeme (Operator op) = showOperator op
  showLexeme (Number w) = show w

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

example :: Seq Lexeme
example = Maybe.fromJust $ parseLexemes "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

input :: IO (Seq (Seq Lexeme))
input = Advent2020.Input.loadInput parseLexemes "day18/input"

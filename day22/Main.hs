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
import Control.Monad (when)
import Control.Monad.Trans.Accum (Accum)
import Control.Monad.Trans.Accum qualified as AccumT
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as ExceptT
import Data.Bifunctor (Bifunctor (bimap, second))
import Data.Either qualified as Either
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Read qualified as Read

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show $ solve1 example
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show $ solve2 example
  putStrLn . ("for real: " <>) . show . solve2 =<< input

solve1 :: GameState -> (Player, Word)
solve1 =
  second score
    . head
    . Either.lefts
    . List.iterate' (>>= standardGameRound)
    . pure

solve2 :: GameState -> (Player, Word)
solve2 = second score . newGame
 where
  newGame :: GameState -> (Player, Seq Word)
  newGame gs =
    Either.fromLeft (Player1, player1 gs)
      . flip AccumT.evalAccum (Set.empty :: Set GameState)
      . ExceptT.runExceptT @(Player, Seq Word)
      $ gameLoop gs
  gameLoop :: GameState -> ExceptT (Player, Seq Word) (Accum (Set GameState)) a
  gameLoop gameState = do
    checkWinCondition gameState
    recordCurrentState gameState
    newGameState <- ExceptT.except (gameRound gameState)
    gameLoop newGameState
  checkWinCondition gameState@GameState{player1, player2} = do
    when (null player1) $ ExceptT.throwE (Player2, player2)
    when (null player2) $ ExceptT.throwE (Player1, player1)
    isWinForPlayer1 <- Trans.lift $ AccumT.looks (gameState `Set.member`)
    when isWinForPlayer1 $
      ExceptT.throwE (Player1, player1)
  recordCurrentState = Trans.lift . AccumT.add . Set.singleton
  gameRound :: GameRound
  gameRound GameState{player1 = Seq.Empty, player2} = Left (Player2, player2)
  gameRound GameState{player1, player2 = Seq.Empty} = Left (Player1, player1)
  gameRound GameState{player1 = (p1 :<| moreP1), player2 = (p2 :<| moreP2)} =
    case whoWinsTheRound p1 moreP1 p2 moreP2 of
      Player1 -> Right $ GameState (moreP1 :|> p1 :|> p2) moreP2
      Player2 -> Right $ GameState moreP1 (moreP2 :|> p2 :|> p1)
  whoWinsTheRound p1 moreP1 p2 moreP2
    | fromIntegral p1 > Seq.length moreP1 || fromIntegral p2 > Seq.length moreP2 =
      if p1 >= p2 then Player1 else Player2
    | otherwise = recursiveCombat (Seq.take (fromIntegral p1) moreP1) (Seq.take (fromIntegral p2) moreP2)
  recursiveCombat :: Seq Word -> Seq Word -> Player
  recursiveCombat player1 player2 = fst $ newGame (GameState player1 player2)

-- | A GameRound either returns the winning deck, or the next GameState
type GameRound = GameState -> Either (Player, Seq Word) GameState

score :: Foldable t => t Word -> Word
score = sum . zipWith (*) [1 ..] . reverse . Foldable.toList

standardGameRound :: GameRound
standardGameRound GameState{player1 = Seq.Empty, player2} = Left (Player2, player2)
standardGameRound GameState{player1, player2 = Seq.Empty} = Left (Player1, player1)
standardGameRound GameState{player1 = (p1 :<| moreP1), player2 = (p2 :<| moreP2)} =
  case compare p1 p2 of
    GT -> Right $ GameState (moreP1 :|> p1 :|> p2) moreP2 -- p1 wins
    LT -> Right $ GameState moreP1 (moreP2 :|> p2 :|> p1) -- p2 wins
    EQ -> Right $ GameState (moreP1 :|> p1) (moreP2 :|> p2) -- draw

data Player = Player1 | Player2
  deriving stock (Eq, Ord, Show)
data GameState = GameState
  { player1 :: Seq Word
  , player2 :: Seq Word
  }
  deriving stock (Eq, Ord, Show)

parseGameState :: Foldable t => t String -> GameState
parseGameState =
  uncurry GameState
    . bimap parseCards parseCards
    . break (== "")
    . Foldable.toList
 where
  parseCards :: [String] -> Seq Word
  parseCards = Seq.fromList . Maybe.mapMaybe (Read.readMaybe @Word)

-- >>> example
-- GameState {player1 = fromList [9,2,6,3,1], player2 = fromList [5,8,4,7,10]}

example :: GameState
example =
  parseGameState
    [ "Player 1:"
    , "9"
    , "2"
    , "6"
    , "3"
    , "1"
    , ""
    , "Player 2:"
    , "5"
    , "8"
    , "4"
    , "7"
    , "10"
    ]

input :: IO GameState
input = parseGameState <$> Advent2020.Input.loadInput Just "day22/input"

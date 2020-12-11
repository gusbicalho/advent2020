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

module Advent2020.Day08 (day08_01, day08_02) where

import Advent2020.Input qualified
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe qualified as MaybeT
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Functor.Identity qualified as Identity
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Numeric.Natural (Natural)
import Text.Read qualified as Read
import qualified Data.Set as Set
import Control.Monad (forever)

day08_01 :: IO Int
day08_01 = solve01 <$> input

solve01 :: Seq Op -> Int
solve01 ops =
  unAccumulator . evalStack . forever $ do
    whenM hasVisitedCurrentBefore $
      halt
    recordVisitedCurrent
    (Instruction instruction) <- currentInstruction
    case ops Seq.!? instruction of
      Nothing -> halt
      Just (Jmp n) -> jmp n
      Just (Acc n) -> acc n *> next
      Just Nop -> next
    pure ()
 where
  hasVisitedCurrentBefore :: Solve01M Bool
  hasVisitedCurrentBefore = do
    current <- currentInstruction
    visited <- lift StateT.get
    pure $ current `Set.member` visited
  recordVisitedCurrent :: Solve01M ()
  recordVisitedCurrent = do
    current <- currentInstruction
    lift . StateT.modify' $ Set.insert current
  currentInstruction :: Solve01M Instruction
  currentInstruction = lift . lift $ StateT.get
  next :: Solve01M ()
  next = jmp 1
  halt :: Solve01M ()
  halt = MaybeT.MaybeT $ pure Nothing
  jmp :: Int -> Solve01M ()
  jmp n = lift . lift . StateT.modify' $ \(Instruction i) -> Instruction (i + n)
  acc :: Int -> Solve01M ()
  acc n = lift . lift . lift . StateT.modify' $ \(Accumulator i) -> Accumulator (i + n)
  evalStack :: Solve01M () -> Accumulator
  evalStack =
    flip StateT.execState (Accumulator 0)
      . flip StateT.execStateT (Instruction 0)
      . flip StateT.execStateT Set.empty
      . MaybeT.runMaybeT

whenM :: Monad m => m Bool -> m () -> m ()
whenM condM action = condM >>= \case
  True -> action
  False -> pure ()

type Solve01M a =
  MaybeT.MaybeT
    ( StateT.StateT
        (Set Instruction)
        ( StateT.StateT
            Instruction
            (StateT.State Accumulator)
        )
    )
    a
newtype Accumulator = Accumulator {unAccumulator :: Int}
newtype Instruction = Instruction Int
  deriving newtype (Eq, Ord)

-- >>> solve01 $ example
-- 5

-- >>> day08_01
-- 1594

day08_02 :: IO Natural
day08_02 = solve02 <$> input

solve02 :: Seq Op -> Natural
solve02 _input = 0

-- >>> solve02 $ example

-- >>> day08_02

data Op = Nop | Acc Int | Jmp Int
  deriving stock (Eq, Show)

parseOp :: String -> Maybe Op
parseOp ('n' : 'o' : 'p' : ' ' : _) = Just Nop
parseOp ('a' : 'c' : 'c' : ' ' : num) = Acc <$> readSignedInt num
parseOp ('j' : 'm' : 'p' : ' ' : num) = Jmp <$> readSignedInt num
parseOp _ = Nothing

readSignedInt :: String -> Maybe Int
readSignedInt [] = Nothing
readSignedInt (sign : num) =
  case sign of
    '-' -> negate <$> unsigned
    '+' -> unsigned
    _ -> Nothing
 where
  unsigned = Read.readMaybe num

example :: Seq Op
example =
  Seq.fromList . Maybe.mapMaybe parseOp $
    [ "nop +0"
    , "acc +1"
    , "jmp +4"
    , "acc +3"
    , "jmp -3"
    , "acc -99"
    , "acc +1"
    , "jmp -4"
    , "acc +6"
    ]

input :: IO (Seq Op)
input = Advent2020.Input.loadInput parseOp "resources/day08/input"

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Advent2020.Day08 (day08_01, day08_02) where

import Advent2020.Input qualified
import Data.Foldable qualified as Foldable
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Text.Read qualified as Read

day08_01 :: IO Int
day08_01 = solve01 <$> input

solve01 :: Seq Op -> Int
solve01 = unAccumulator . fst . runOps

-- >>> solve01 $ example
-- 5

-- >>> day08_01
-- 1594

day08_02 :: IO (Maybe Int)
day08_02 = solve02 <$> input

solve02 :: Seq Op -> Maybe Int
solve02 =
  fmap (unAccumulator . fst)
    . Foldable.find halted
    . fmap runOps
    . possibleSwaps swapJmpAndNop
 where
  swapJmpAndNop (Jmp n) = Just $ Nop n
  swapJmpAndNop (Nop n) = Just $ Jmp n
  swapJmpAndNop _ = Nothing
  possibleSwaps swap ops = flip Maybe.mapMaybe [0 .. Seq.length ops] $ \ix -> do
    op <- ops Seq.!? ix
    swapped <- swap op
    pure $ Seq.update ix swapped ops
  halted (_, Halt) = True
  halted _ = False

-- >>> solve02 $ example
-- Just 8

-- >>> day08_02
-- Just 758

runOps :: Seq Op -> (Accumulator, Exit)
runOps ops = go Set.empty (Instruction 0) (Accumulator 0)
 where
  go visited current@(Instruction currentIx) acc
    | current `Set.member` visited = (acc, Loop)
    | Just op <- ops Seq.!? currentIx = runOp op current acc (go (Set.insert current visited))
    | currentIx == Seq.length ops = (acc, Halt)
    | otherwise = (acc, BadJump)

runOp :: Op -> Instruction -> Accumulator -> (Instruction -> Accumulator -> k) -> k
runOp op current acc k = case op of
  Jmp n -> k (jmp n current) acc
  Acc n -> k (next current) (add n acc)
  Nop _ -> k (next current) acc
 where
  next = jmp 1
  jmp n (Instruction i) = Instruction (i + n)
  add n (Accumulator a) = Accumulator (a + n)

newtype Accumulator = Accumulator {unAccumulator :: Int}
newtype Instruction = Instruction Int
  deriving stock (Eq, Ord)
data Exit = Loop | BadJump | Halt

data Op = Nop Int | Acc Int | Jmp Int
  deriving stock (Eq, Show)

parseOp :: String -> Maybe Op
parseOp ('n' : 'o' : 'p' : ' ' : num) = Nop <$> readSignedInt num
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

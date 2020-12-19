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
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Advent2020.Input qualified
import Data.Bits (Bits (bit, clearBit, complement, setBit, testBit, zeroBits, (.&.), (.|.)), FiniteBits (finiteBitSize))
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Text.Read qualified as Read

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show $ solve1 example
  putStrLn . ("for real: " <>) . show . solve1 =<< input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show $ solve2 example2
  putStrLn . ("for real: " <>) . show . solve2 =<< input

-- >>> solve1 example
-- 165

solve1 :: Seq Op -> Word
solve1 = runProtocol writerV1
 where
  writerV1 :: WriteFn []
  writerV1 mask address value = [(address, applyMask mask value)]
  applyMask :: Mask -> Word -> Word
  applyMask Mask{maskZeroes, maskOnes} =
    (.|. maskOnes) . (.&. maskZeroes)

-- >>> solve2 example2
-- 208

solve2 :: Seq Op -> Word
solve2 = runProtocol writeV2
 where
  writeV2 :: WriteFn []
  writeV2 mask address value = applyMask mask address <&> (,value)
  applyMask :: Mask -> Word -> [Word]
  applyMask Mask{maskZeroes, maskOnes} w =
    let w' = w .|. maskOnes
        -- floaters are bits which are CLEAR in maskOnes and SET in maskZeroes
        floatersWord = complement maskOnes .&. maskZeroes
        floaterBitIndexes = filter (testBit floatersWord) [0 .. finiteBitSize (0 :: Word)]
     in floatBits w' floaterBitIndexes

floatBits :: Word -> [Int] -> [Word]
floatBits w [] = [w]
floatBits w (i : floaters) = do
  w' <- floatBits w floaters
  [setBit w' i, clearBit w' i]

-- wToBits :: Word -> String
-- wToBits w = Bool.bool '0' '1' . testBit w <$> reverse [0 .. finiteBitSize (0 :: Word)]

type WriteFn t = Mask -> Word -> Word -> t (Word, Word)

runProtocol :: (Foldable t1, Foldable t2) => WriteFn t2 -> t1 Op -> Word
runProtocol writeFn = memoryTotal . Foldable.foldl' step newDevice
 where
  memoryTotal = sum . Map.elems . memory
  step device (OpMask newMask) = device{mask = newMask}
  step Device{mask, memory} (OpWrite address value) =
    let writes = writeFn mask address value
     in Device
          { mask = mask
          , memory = Foldable.foldl' (\m (a, v) -> Map.insert a v m) memory writes
          }

newDevice :: Device
newDevice = Device emptyMask Map.empty

data Device = Device
  { mask :: Mask
  , memory :: Map Word Word
  }
  deriving stock (Eq, Ord, Show)

emptyMask :: Mask
emptyMask = Mask zeroBits (complement zeroBits)

data Mask = Mask
  { maskZeroes :: Word
  , maskOnes :: Word
  }
  deriving stock (Eq, Ord, Show)

data Op = OpMask Mask | OpWrite Word Word
  deriving stock (Eq, Ord, Show)

parseOp :: String -> Maybe Op
parseOp ('m' : 'a' : 's' : 'k' : ' ' : '=' : ' ' : maskStr) = Just . OpMask $ goMask maxBits 0 0 maskStr
 where
  maxBits = 35
  clearExtraLeadingBits :: Word -> Word
  clearExtraLeadingBits w = Foldable.foldl' clearBit w [maxBits + 1 .. finiteBitSize w]
  goMask :: Int -> Word -> Word -> String -> Mask
  goMask _ zeroes ones [] = Mask (clearExtraLeadingBits $ complement zeroes) ones
  goMask i zeroes ones ('1' : digits) = goMask (i -1) zeroes (ones .|. bit i) digits
  goMask i zeroes ones ('0' : digits) = goMask (i -1) (zeroes .|. bit i) ones digits
  goMask i zeroes ones (_ : digits) = goMask (i -1) zeroes ones digits
parseOp ('m' : 'e' : 'm' : '[' : s) = do
  (address, s) <- readDigits @Word s
  s <- match "] = " s
  (value, _) <- readDigits @Word s
  pure $ OpWrite address value
parseOp _ = Nothing

readDigits :: forall n. Read n => String -> Maybe (n, String)
readDigits s = case span Char.isDigit s of
  (Read.readMaybe @n -> Just n, rest) -> Just (n, rest)
  _ -> Nothing

match :: Eq a => [a] -> [a] -> Maybe [a]
match expected s = case splitAt (length expected) s of
  (prefix, rest)
    | prefix == expected -> Just rest
    | otherwise -> Nothing

example :: Seq Op
example =
  Seq.fromList . Maybe.mapMaybe parseOp $
    [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    , "mem[8] = 11"
    , "mem[7] = 101"
    , "mem[8] = 0"
    ]

example2 :: Seq Op
example2 =
  Seq.fromList . Maybe.mapMaybe parseOp $
    [ "mask = 000000000000000000000000000000X1001X"
    , "mem[42] = 100"
    , "mask = 00000000000000000000000000000000X0XX"
    , "mem[26] = 1"
    ]

input :: IO (Seq Op)
input = Advent2020.Input.loadInput parseOp "day14/input"

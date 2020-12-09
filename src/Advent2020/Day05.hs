{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Advent2020.Day05 (day05_01, day05_02) where

import Advent2020.Input qualified
import Data.Array.Unboxed qualified as UArray
import Data.Bits (Bits (shiftL), (.|.))
import Data.Foldable (find, toList, Foldable (foldl'))
import Data.Semigroup (Max (Max), getMax)
import Data.Sequence (Seq)
import Data.Word (Word16)

day05_01 :: IO Word16
day05_01 = solve01 <$> inputWords

solve01 :: Seq Word16 -> Word16
solve01 = maximum

-- >>> day05_01
-- 850

day05_02 :: IO (Maybe Word16)
day05_02 = solve02 <$> inputWords

solve02 :: Seq Word16 -> Maybe Word16
solve02 is = find freeSeat . dropWhile freeSeat $ [1..maxSeat]
 where
  maxSeat = 2 ^ (10 :: Word16)
  occupiedSeats :: UArray.UArray Word16 Bool
  occupiedSeats =
    UArray.accumArray (||) False (0, maxSeat)
      . map (,True)
      . toList
      $ is
  freeSeat seat = not (occupiedSeats UArray.! seat)

-- >>> day05_02
-- Just 599

inputWords :: IO (Seq Word16)
inputWords = Advent2020.Input.loadInput (Just . parseSeatId) "resources/day05/input"

-- >>> parseSeatId "FBFBBFFRLR"
-- 357

parseSeatId :: [Char] -> Word16
parseSeatId = foldl' add 0 . fmap toBitInc
 where
  toBitInc :: Char -> (Word16 -> Word16)
  toBitInc 'B' = (.|. 1)
  toBitInc 'R' = (.|. 1)
  toBitInc _ = id
  add :: Word16 -> (Word16 -> Word16) -> Word16
  add n inc = inc $ shiftL n 1

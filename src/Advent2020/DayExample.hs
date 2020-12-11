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

module Advent2020.DayExample (dayExample_01, dayExample_02) where

import Advent2020.Input qualified
import Data.Sequence (Seq)
import Numeric.Natural (Natural)
import qualified Data.Sequence as Seq

dayExample_01 :: IO Natural
dayExample_01 = solve01 <$> input

solve01 :: Seq String -> Natural
solve01 _input = 0
-- >>> solve01 $ example

-- >>> dayExample_01

dayExample_02 :: IO Natural
dayExample_02 = solve02 <$> input

solve02 :: Seq String -> Natural
solve02 _input = 0
-- >>> solve02 $ example

-- >>> dayExample_02

type Input = String

example :: Seq Input
example =
  Seq.fromList []

input :: IO (Seq Input)
input = Advent2020.Input.loadInput Just "resources/dayExample/input"

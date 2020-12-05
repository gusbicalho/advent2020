{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Advent2020.Day02 (day02_01) where

import Advent2020.Input qualified
import Data.Char qualified as Char
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Numeric.Natural (Natural)
import Text.Read (readMaybe)
import qualified Data.List as List

example :: Seq InputRecord
example =
  Seq.fromList
    [ InputRecord (PasswordPolicy 1 3 'a') "abcde"
    , InputRecord (PasswordPolicy 1 3 'b') "cdefg"
    , InputRecord (PasswordPolicy 2 9 'c') "ccccccccc"
    ]

loadInput :: IO (Seq InputRecord)
loadInput =
  Advent2020.Input.loadInput parseInputLine "resources/day02/input"

-- >>> solve example
-- Response {unResponse = 2}

-- >>> day02_01
-- Response {unResponse = 655}

day02_01 :: IO Response
day02_01 = solve validateMinMax <$> loadInput

-- >>> day02_02
-- Response {unResponse = 673}

day02_02 :: IO Response
day02_02 = solve validatePlaces <$> loadInput

solve :: Validation -> Seq InputRecord -> Response
solve validate = Response . fromIntegral . Seq.length . Seq.filter check
 where
  check (InputRecord policy password) = validate policy password

validateMinMax :: Validation
validateMinMax (PasswordPolicy minTimes maxTimes char) s =
  let count = List.genericLength . filter (char ==) $ s
  in count >= minTimes && count <= maxTimes

validatePlaces :: Validation
validatePlaces (PasswordPolicy placeA placeB char) s =
  match placeA /= match placeB
 where
  match n = case drop (fromIntegral n - 1) s of
    (c : _) | c == char -> True
    _ -> False

type Validation = PasswordPolicy -> String -> Bool

data InputRecord = InputRecord
  { irPolicy :: PasswordPolicy
  , irPassword :: String
  }
  deriving stock (Show)
data PasswordPolicy = PasswordPolicy
  { pwpPlaceA :: Natural
  , pwpPlaceB :: Natural
  , pwpChar :: Char
  }
  deriving stock (Show)

newtype Response = Response {unResponse :: Integer}
  deriving stock (Eq, Show)

parseInputLine :: String -> Maybe InputRecord
parseInputLine s = do
  (placeAStr, s) <- pure $ span Char.isNumber s
  placeA <- readMaybe @Natural placeAStr
  ('-' : s) <- pure s
  (placeBStr, s) <- pure $ span Char.isNumber s
  placeB <- readMaybe @Natural placeBStr
  let (' ' : char : ':' : ' ' : password) = s
  pure $ InputRecord (PasswordPolicy placeA placeB char) password

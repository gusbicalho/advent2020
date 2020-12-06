{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Advent2020.Day04 (day04_01, day04_02) where

import Advent2020.Input qualified
import Data.Char qualified as Char
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Read (readMaybe)

day04_01 :: IO Int
day04_01 = solve01 <$> inputLines

solve01 :: Seq String -> Int
solve01 =
  length
  . filter validatePassport
  . fmap (foldMap singleField)
  . parsePairs
  . toList

-- >>> solve01 exampleLines
-- 2

-- >>> day04_01
-- 190

day04_02 :: IO Int
day04_02 = solve02 <$> inputLines

solve02 :: Seq String -> Int
solve02 =
  length
    . filter validatePassport
    . fmap (foldMap singleField . filter (uncurry validateField))
    . parsePairs
    . toList

-- >>> solve02 exampleLines
-- 2

-- >>> solve02 validExamples
-- 4

-- >>> day04_02
-- 121

parsePairs :: [String] -> [[(String, String)]]
parsePairs input =
  input
    & splitOn ("" ==)
    <&> foldMap (" " <>)
    <&> splitPairs
 where
  splitOn :: (a -> Bool) -> [a] -> [[a]]
  splitOn isSeparator xs =
    case break isSeparator $ dropWhile isSeparator xs of
      (group, []) -> [group]
      (group, restOfInput) -> group : splitOn isSeparator restOfInput
  splitPairs :: String -> [(String, String)]
  splitPairs = foldMap (toList . splitPair) . splitOn (' ' ==)
  splitPair :: String -> Maybe (String, String)
  splitPair s = case break (':' ==) s of
    (propName@(_ : _), ':' : propVal) -> Just (propName, propVal)
    _ -> Nothing

validateField :: String -> String -> Bool
validateField "byr" = validYear "1920" "2002"
validateField "iyr" = validYear "2010" "2020"
validateField "eyr" = validYear "2020" "2030"
validateField "hgt" = \s -> case span Char.isDigit s of
  (readMaybe @Int -> Just num, "cm") -> 150 <= num && num <= 193
  (readMaybe @Int -> Just num, "in") -> 59 <= num && num <= 76
  _ -> False
validateField "hcl" = \case
  ('#' : hex) -> hasLength 6 hex && all Char.isHexDigit hex
  _ -> False
validateField "ecl" = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
validateField "pid" = \s -> hasLength 9 s && all Char.isDigit s
validateField "cid" = const True
validateField _ = const False

validYear :: String -> String -> String -> Bool
validYear minYear maxYear year@[_, _, _, _] = minYear <= year && year <= maxYear
validYear _ _ _ = False

hasLength :: Int -> [a] -> Bool
hasLength n [] = n == 0
hasLength 0 _ = False
hasLength n (_ : xs) = hasLength (n -1) xs

validatePassport :: PassportInfo -> Bool
validatePassport (PassportInfo fields) =
  requiredFields `Set.isSubsetOf` fields
 where
  requiredFields =
    Set.fromList
      [ "byr"
      , "iyr"
      , "eyr"
      , "hgt"
      , "hcl"
      , "ecl"
      , "pid"
      ]

singleField :: (String, String) -> PassportInfo
singleField (key, _) = PassportInfo (Set.singleton key)

newtype PassportInfo = PassportInfo (Set String)
  deriving stock (Show)
  deriving newtype (Monoid, Semigroup)

exampleLines :: Seq String
exampleLines =
  Seq.fromList
    [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
    , "byr:1937 iyr:2017 cid:147 hgt:183cm"
    , ""
    , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
    , "hcl:#cfa07d byr:1929"
    , ""
    , "hcl:#ae17e1 iyr:2013"
    , "eyr:2024"
    , "ecl:brn pid:760753108 byr:1931"
    , "hgt:179cm"
    , ""
    , "hcl:#cfa07d eyr:2025 pid:166559648"
    , "iyr:2011 ecl:brn hgt:59in"
    ]

validExamples :: Seq String
validExamples =
  Seq.fromList
    [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
    , "hcl:#623a2f"
    , ""
    , "eyr:2029 ecl:blu cid:129 byr:1989"
    , "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
    , ""
    , "hcl:#888785"
    , "hgt:164cm byr:2001 iyr:2015 cid:88"
    , "pid:545766238 ecl:hzl"
    , "eyr:2022"
    , ""
    , "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
    ]

inputLines :: IO (Seq String)
inputLines = Advent2020.Input.inputLines "resources/day04/input"

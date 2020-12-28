{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.Bifunctor (Bifunctor (second))
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Word (Word64)
import qualified Data.Tuple as Tuple

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn . ("example: " <>) . show . solve1 $ example
  putStrLn . ("for real: " <>) . show . solve1 $ input
  putStrLn "Part 2"
  putStrLn . ("example: " <>) . show $ solve2 example
  putStrLn . ("for real: " <>) . show $ solve2 input

solve1 :: Circle Word64 -> String
solve1 = toAnswer . snd . (!! 100) . List.iterate' move . (0,) . circleTake 9
 where
  toAnswer circle
    | circle !!* 0 == 1 = drop 1 $ Foldable.foldMap' show circle
    | otherwise = toAnswer (shiftTo 1 circle)

solve2 = toAnswer . snd . (!! 10_000_000) . List.iterate' move . (0,)
 where
  toAnswer circle =
    let ix = Maybe.fromMaybe 0 $ circleIndexOf circle 1
        a = circle !!* (ix + 1)
        b = circle !!* (ix + 2)
     in (ix, a, b, a * b)

-- >>> move . move $ (0, example)
-- (1,Circle (fromList [3,2,8,9,1,5,4,6,7]))

-- >>> extract 2 3 (Circle (Seq.fromList [3,2,8,9,1,5,4,6,7]))
-- (Circle (fromList [3,2,5,4,6,7]),[8,9,1])

move :: (Int, Circle Word64) -> (Int, Circle Word64)
move (currentIx, circle) =
  circle''
    `seq` (fromIntegral $ (currentIx + 1) `modulus` totalSize, circle'')
 where
  totalSize = circleSize circle
  currentLabel = circle !!* currentIx
  !(circle', !extracted) = extract (currentIx + 1) 3 circle
  !destinationLabel = avoidExtracted (labelMinus1 currentLabel)
  -- destinationIx = Maybe.fromMaybe currentIx $ circleIndexOf circle' destinationLabel
  circle'' =
    fixCurrent $ insertAllAfterElement destinationLabel extracted circle'
  fixCurrent c
    | c !!* currentIx == currentLabel = c
    | otherwise = fixCurrent (shift1 c)
  labelMinus1 l = 1 + ((fromIntegral l - 2) `modulus` totalSize)
  avoidExtracted l
    | l `notElem` extracted = l
    | otherwise = avoidExtracted (labelMinus1 l)

extract :: Int -> Int -> Circle a -> (Circle a, Seq a)
extract from count circle@(Circle s)
  | count <= 0 = (circle, Seq.empty)
  | otherwise =
    let (front, (middle, back)) = second (Seq.splitAt count) . Seq.splitAt from $ s
        missingPieceSize = count - Seq.length middle
        (frontTaken, frontStay) = Seq.splitAt missingPieceSize front
     in (Circle $ frontStay <> back, middle <> frontTaken)

-- (!!*!) :: Circle a -> Int -> (Circle a, a)
-- c@(Circle s) !!*!

(!*) :: Circle a -> Int -> (Circle a, a)
c@(Circle s) !* i = (Circle $ Seq.deleteAt iMod s, Seq.index s iMod)
 where
  iMod = fromIntegral $ i `modulus` circleSize c

(!!*) :: Circle a -> Int -> a
c !!* i = snd $ c !* i

insertAllAfterElement :: (Eq a) => a -> Seq a -> Circle a -> Circle a
insertAllAfterElement reference as c@(Circle s) =
  case Seq.elemIndexL reference s of
    Nothing -> c
    Just ix ->
      let !(front, back) = Seq.splitAt (succ ix) s
      in Circle $ front <> as <> back

circleIndexOf :: Eq a => Circle a -> a -> Maybe Int
circleIndexOf (Circle s) e = Seq.elemIndexL e s

circleSize :: Circle a -> Word64
circleSize (Circle s) = fromIntegral $ Seq.length s

circleTake n (Circle s) = Circle (Seq.take n s)

shift1 :: Circle a -> Circle a
shift1 (Circle Seq.Empty) = Circle Seq.Empty
shift1 (Circle (e :<| more)) = Circle (more :|> e)

shiftTo :: Eq a => a -> Circle a -> Circle a
shiftTo e c@(Circle s) = case Seq.elemIndexL e s of
  Nothing -> c
  Just ix -> Circle . uncurry (<>) . Tuple.swap . Seq.splitAt ix $ s

modulus :: Int -> Word64 -> Word64
modulus i m = case compare 0 i of
  EQ -> 0
  LT -> fromIntegral i `mod` m
  GT ->
    let m' = fromIntegral m
     in modulus (m' + (i `rem` m')) m

-- >>> (id &&& flip modulus 3) <$> [-5..5]
-- [(-5,1),(-4,2),(-3,0),(-2,1),(-1,2),(0,0),(1,1),(2,2),(3,0),(4,1),(5,2)]

newtype Circle a = Circle (Seq a)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Foldable)

example :: Circle Word64
example =
  Circle . Seq.fromList $
    [3, 8, 9, 1, 2, 5, 4, 6, 7] <> [10 .. 999_999]

input :: Circle Word64
input =
  Circle . Seq.fromList $
    [2, 1, 5, 6, 9, 4, 7, 8, 3] <> [10 .. 999_999]

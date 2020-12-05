{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Advent2020.Day01 (day01_01, day01_02) where

import Advent2020.BoundedSet qualified as BoundedSet
import Advent2020.Input qualified
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Functor.Identity qualified as Identity
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Text.Read (readMaybe)

example :: Seq Integer
example = Seq.fromList [1721, 979, 366, 299, 675, 1456]

loadInput :: IO (Seq Integer)
loadInput =
  Advent2020.Input.loadInput
    (readMaybe @Integer)
    "resources/day01/input"

newtype Response = Response {unResponse :: Maybe Integer}
  deriving stock (Eq, Show)

day01_01 :: IO Response
day01_01 = solve01 <$> loadInput

-- >>> day01_01
-- Response {unResponse = Just 1016964}

-- >>> solve01 example
-- Response {unResponse = Just 514579}

solve01 :: Seq Integer -> Response
solve01 =
  Response
    . fmap (uncurry (*))
    . Identity.runIdentity
    . closingIn checkSums2020
    . Seq.sort
 where
  checkSums2020 firstElement lastElement =
    pure $ case compare (firstElement + lastElement) 2020 of
      EQ -> Done (firstElement, lastElement)
      GT -> DropLast
      LT -> DropFirst

day01_02 :: IO Response
day01_02 = solve02 <$> loadInput

-- >>> solve02 <$> loadInput
-- Response {unResponse = Just 182588480}

-- >>> solve02 example
-- Response {unResponse = Just 241861950}

solve02 :: Seq Integer -> Response
solve02 =
  Response
    . fmap (\(a, b, c) -> a * b * c)
    . ( \case
          Seq.Empty -> Nothing
          (first Seq.:<| rest) ->
            flip StateT.evalState (BoundedSet.singleton first)
              . closingIn checkSums2020
              $ rest
      )
    . Seq.sort
 where
  checkSums2020 firstElement lastElement = do
    smalls <- StateT.get
    let missingThird = 2020 - (firstElement + lastElement)
    case ( BoundedSet.checkBounds smalls missingThird
         , missingThird `BoundedSet.elem` smalls
         ) of
      (LT, _) -> pure DropLast
      (GT, _) -> do
        StateT.modify' $ BoundedSet.insert firstElement
        pure DropFirst
      (EQ, True) -> pure $ Done (missingThird, firstElement, lastElement)
      (EQ, False) -> do
        -- If the missingThird existed, we would have found it by now
        -- therefore, we can drop all the previous numbers
        StateT.put $ BoundedSet.singleton firstElement
        pure DropFirst

data ClosingInControl a = Done a | DropLast | DropFirst
closingIn :: Monad m => (a1 -> a1 -> m (ClosingInControl a2)) -> Seq a1 -> m (Maybe a2)
closingIn
  f
  ( front@( firstElement
              Seq.:<| middle
            )
      Seq.:|> lastElement
    ) =
    f firstElement lastElement >>= \case
      Done a -> pure $ Just a
      DropLast -> closingIn f front
      DropFirst -> closingIn f $ middle Seq.|> lastElement
closingIn _ _ = pure Nothing

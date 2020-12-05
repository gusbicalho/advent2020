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
import Control.Exception ( catch, throwIO, IOException )
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe qualified as MaybeT
import Control.Monad.Trans.State.Strict qualified as StateT
import Control.Monad.Trans.Writer.CPS qualified as WriterT
import Data.Functor.Identity qualified as Identity
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import System.IO qualified as IO
import System.IO.Error qualified as IO.Error
import Text.Read (readMaybe)

inputFile :: FilePath
inputFile = "resources/day01/input"

newtype Response = Response {unResponse :: Maybe Integer}
  deriving stock (Eq, Show)

day01_01 :: IO Response
day01_01 = solve01 <$> getInput

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
day01_02 = solve02 <$> getInput

-- >>> solve02 <$> getInput
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

example :: Seq Integer
example = Seq.fromList [1721, 979, 366, 299, 675, 1456]

getInput :: IO (Seq Integer)
getInput = IO.withFile inputFile IO.ReadMode $ \inputHandle ->
  WriterT.execWriterT . MaybeT.runMaybeT . forever $ do
    line <-
      stopOnNothing . liftIO $
        (Just <$> IO.hGetLine inputHandle)
          `catch` \(ex :: IOException) ->
            if IO.Error.isEOFError ex
              then pure Nothing
              else throwIO ex
    num <- readMaybeT @Integer line
    lift . WriterT.tell $ Seq.singleton num
 where
  stopOnNothing a =
    a >>= \case
      Nothing -> stop
      Just v -> pure v
  stop :: forall a m. Applicative m => MaybeT.MaybeT m a
  stop = MaybeT.MaybeT $ pure Nothing
  readMaybeT :: forall a m. (Applicative m, Read a) => String -> MaybeT.MaybeT m a
  readMaybeT = MaybeT.MaybeT . pure . readMaybe

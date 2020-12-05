{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Advent2020.Day01 (day01_01, day01_02) where

import Control.Exception
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe qualified as MaybeT
import Control.Monad.Trans.Writer.CPS qualified as WriterT
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import System.IO qualified as IO
import System.IO.Error qualified as IO.Error
import Text.Read (readMaybe)

inputFile :: FilePath
inputFile = "resources/day01/input"

newtype Resp01 = Resp01 {unResp01 :: Maybe Integer}
  deriving stock (Eq, Show)

day01_01 :: IO Resp01
day01_01 = solve01 <$> getInput

solve01 :: Seq Integer -> Resp01
solve01 = Resp01 . fmap (uncurry (*)) . findPairSumming . Seq.sort
 where
  findPairSumming
    ( front@( firstElement
                Seq.:<| middle
              )
        Seq.:|> lastElement
      ) =
      case compare (firstElement + lastElement) 2020 of
        EQ -> Just (firstElement, lastElement)
        GT -> findPairSumming front
        LT -> findPairSumming $ middle Seq.|> lastElement
  findPairSumming _ = Nothing

day01_02 :: [Char]
day01_02 = "day01_02"

getInput :: IO (Seq Integer)
getInput = IO.withFile inputFile IO.ReadMode $
  \inputHandle ->
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

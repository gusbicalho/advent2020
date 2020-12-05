{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent2020.Input where

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (forever, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe qualified as MaybeT
import Control.Monad.Trans.Writer.CPS qualified as WriterT
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import System.IO qualified as IO
import System.IO.Error qualified as IO.Error

loadInput :: (String -> Maybe a) -> FilePath -> IO (Seq a)
loadInput parseLine inputFile = IO.withFile inputFile IO.ReadMode $ \inputHandle ->
  WriterT.execWriterT . MaybeT.runMaybeT . forever $ do
    line <- nextLine inputHandle
    num <- MaybeT.MaybeT . pure . parseLine $ line
    lift . WriterT.tell $ Seq.singleton num
 where
  nextLine inputHandle =
    maybe stop pure <=< liftIO $
      (Just <$> IO.hGetLine inputHandle)
        `catch` \(ex :: IOException) ->
          if IO.Error.isEOFError ex
            then pure Nothing
            else throwIO ex
  stop = MaybeT.MaybeT $ pure Nothing

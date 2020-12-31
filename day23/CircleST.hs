{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}

module CircleST (
  CircleST,
  focus,
  minimum,
  maximum,
  focusOn,
  shiftCw,
  takeCw,
  disconnect,
  connectCwOf,
  toListCw,
  seed,
) where

import Control.Monad.ST.Strict (ST)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.STRef.Strict (STRef)
import Data.STRef.Strict qualified as STRef
import Prelude hiding (maximum, minimum)

data Node s a = Node
  { label :: a
  , neighCw :: STRef s (Node s a)
  , neighCcw :: STRef s (Node s a)
  }

data CircleST s a = CircleST
  { nodeIndex :: Map a (Node s a)
  , focusNode :: Node s a
  , minimum :: a
  , maximum :: a
  }

seed :: (Ord a) => [a] -> ST s (Either String (CircleST s a))
seed = \case
  [] -> pure $ Left "Cannot have a Circle with 0 elements"
  (a : as) -> go as =<< singleton a
 where
  go [] c = pure $ Right c
  go (a : as) c@CircleST{nodeIndex} =
    case Map.lookup a nodeIndex of
      Just _ -> pure $ Left "Duplicated element in seed, not allowed"
      Nothing -> go as =<< insert a c
  insert a c@CircleST{nodeIndex, focusNode} = do
    rec aNode <- newNode a aNode aNode
    let newC =
          c
            { nodeIndex = Map.insert a aNode nodeIndex
            , minimum = min a (minimum c)
            , maximum = max a (maximum c)
            }
    ccw <- STRef.readSTRef (neighCcw focusNode)
    connectNodes focusNode aNode
    connectNodes aNode ccw
    pure newC

shiftCw :: CircleST s a -> ST s (CircleST s a)
shiftCw = shift' $ STRef.readSTRef . neighCw

focusOn :: Ord a => a -> CircleST s a -> Either String (CircleST s a)
focusOn label c@CircleST{nodeIndex} =
  case Map.lookup label nodeIndex of
    Nothing -> Left "Target node not found"
    Just node -> Right $ c{focusNode = node}

focus :: CircleST s a -> a
focus = label . focusNode

takeCw :: Int -> CircleST s a -> ST s [a]
takeCw n = take' (STRef.readSTRef . neighCw) n . focusNode

size :: CircleST s k -> Int
size = Map.size . nodeIndex

toListCw :: CircleST s a -> ST s [a]
toListCw c = takeCw (size c) c

-- Impl helpers

singleton :: a -> ST s (CircleST s a)
singleton a = do
  rec node <- newNode a node node
  pure $
    CircleST
      { nodeIndex = Map.singleton a node
      , focusNode = node
      , minimum = a
      , maximum = a
      }

newNode :: a -> Node s a -> Node s a -> ST s (Node s a)
newNode a cw ccw =
  Node a <$> STRef.newSTRef cw <*> STRef.newSTRef ccw

disconnect :: Ord a => a -> CircleST s a -> ST s ()
disconnect label CircleST{nodeIndex} = do
  case Map.lookup label nodeIndex of
    Nothing -> pure ()
    Just node -> disconnectNode node

connectCwOf :: Ord a => a -> a -> CircleST s a -> ST s (Either String ())
connectCwOf moving reference CircleST{nodeIndex} =
  relevantNodes & either (pure . Left) (uncurry moveNode)
 where
  relevantNodes =
    case (Map.lookup moving nodeIndex, Map.lookup reference nodeIndex) of
      (Just movingNode, Just referenceNode) -> Right (movingNode, referenceNode)
      (Nothing, _) -> Left "Moving node not found"
      (_, Nothing) -> Left "Reference node not found"
  moveNode movingNode referenceNode = do
    cwOfReference <- STRef.readSTRef (neighCw referenceNode)
    disconnectNode movingNode
    connectNodes cwOfReference movingNode
    connectNodes movingNode referenceNode
    pure $ Right ()

disconnectNode :: Node s a -> ST s ()
disconnectNode node = do
  cw <- STRef.readSTRef $ neighCw node
  ccw <- STRef.readSTRef $ neighCcw node
  connectNodes cw ccw
  connectNodes node node

connectNodes :: Node s a -> Node s a -> ST s ()
connectNodes cw ccw = do
  STRef.modifySTRef' (neighCcw cw) (const ccw)
  STRef.modifySTRef' (neighCw ccw) (const cw)

take' :: (Node s a -> ST s (Node s a)) -> Int -> Node s a -> ST s [a]
take' step = go
 where
  go n node@Node{label}
    | n <= 0 = pure []
    | otherwise = (label :) <$> (go (n - 1) =<< step node)

shift' :: (Node s a -> ST s (Node s a)) -> CircleST s a -> ST s (CircleST s a)
shift' next c@CircleST{focusNode} = do
  newFocus <- next focusNode
  pure c{focusNode = newFocus}

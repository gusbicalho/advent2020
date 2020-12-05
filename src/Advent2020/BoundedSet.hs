{-# LANGUAGE ImportQualifiedPost #-}

module Advent2020.BoundedSet (
  BoundedSet,
  insert,
  singleton,
  checkBounds,
  Advent2020.BoundedSet.elem
) where

import Data.Foldable qualified as Foldable
import Data.Set (Set)
import Data.Set qualified as Set

data BoundedSet a = BoundedSet
  { bsElements :: Set a
  , bsSmallest :: a
  , bsLargest :: a
  }

singleton :: a -> BoundedSet a
singleton a = BoundedSet (Set.singleton a) a a

instance Foldable BoundedSet where
  foldMap f = foldMap f . bsElements
  foldr f z = foldr f z . bsElements

insert :: Ord a => a -> BoundedSet a -> BoundedSet a
insert a (BoundedSet elements smallest largest) =
  BoundedSet
    (Set.insert a elements)
    (min smallest a)
    (max largest a)

checkBounds :: Ord a => BoundedSet a -> a -> Ordering
checkBounds (BoundedSet _ smallest largest) a =
  case compare a smallest of
    LT -> LT
    EQ -> EQ
    GT -> case compare a largest of
      LT -> EQ
      EQ -> EQ
      GT -> GT

elem :: Eq a => a -> BoundedSet a -> Bool
elem a (BoundedSet elements smallest largest) =
  a == smallest || a == largest || a `Foldable.elem` elements

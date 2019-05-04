module Lattice where

import           Data.Set as Set

class Lattice s where
  union :: s -> s -> s
  difference :: s -> s -> s
  lessThan :: s -> s -> Bool
  intersection :: s -> s -> s

instance (Ord a) => Lattice (Set a) where
  union = Set.union
  difference = Set.difference
  intersection = Set.intersection
  lessThan = Set.isSubsetOf

{-# LANGUAGE GADTs #-}

module DFAFramework where

import           AbstractSyntaxTree
import           ControlFlowGraph
import           Data.Map.Strict    as Map
import           Data.Set           as Set
import           Lattice

data DFA a =
  DFA
    { bottom           :: a
    , flow             :: Set CFNode
    , transferFunction :: Statement -> a -> a
    , graph            :: CFGraph
    }

newtype Result a =
  Result
    { result :: Map Label a
    }

instance (Show a) => Show (Result a) where
  show res = show r
    where
      r = result res

solve :: (Lattice a, Show a) => DFA a -> Result a
solve givenData =
  let go [] acc             = acc
      go ((In l l'):xs) acc = acc
   in Result {result = go workList init}
  where
    controlFlow = flow givenData
    workList = Set.toList controlFlow
    function = transferFunction givenData
    default' = bottom givenData
    controlFlowGraph = graph givenData
    init = Map.empty

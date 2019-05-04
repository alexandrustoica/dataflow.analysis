{-# LANGUAGE GADTs #-}

module DFAFramework where

import           AbstractSyntaxTree
import           ControlFlowGraph   hiding (finals, initial)
import           Data.Map.Strict    as Map hiding (intersection, union)
import           Data.Set           as Set (Set, toList)
import           Lattice

data DFA a =
  DFA
    { bottom           :: a
    , finals           :: [Label]
    , flow             :: Set Edge
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

backward :: (Lattice a, Show a) => DFA a -> Result a
backward givenData =
  let go [] acc = acc
      go (Edge l l':xs) acc =
        if not (new `lessThan` old)
          then let workList' = ControlFlowGraph.withStart l controlFlow ++ xs
                   accumulator = Map.insert l' (new `union` old) acc
                in go workList' accumulator
          else go xs acc
        where
          target = controlFlowGraph ! l
          new = function target (Map.findWithDefault default' l acc)
          old = Map.findWithDefault default' l' acc
   in Result {result = go workList initial}
  where
    controlFlow = flow givenData
    workList = Set.toList controlFlow
    function = transferFunction givenData
    default' = bottom givenData
    controlFlowGraph = graph givenData
    initial = Map.fromList $ zip (finals givenData) (repeat (bottom givenData))

{-# LANGUAGE GADTs #-}

module DFAFramework where

import           AbstractSyntaxTree
import           ControlFlowGraph   hiding (finals, initial)
import           Data.Map.Strict    as Map hiding (difference, intersection,
                                            union)
import           Data.Set           as Set (Set, difference, fromList, toList)
import           Lattice

import           Debug.Trace

data DFA a =
  DFA
    { bottom           :: a
    , finals           :: [Label]
    , flow             :: Set Edge
    , transferFunction :: Statement -> a -> a
    , graph            :: CFGraph
    , program          :: Program
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
      go es@(Edge l l':xs) acc
        | trace
           (show l ++ " -- " ++ (show $ Map.findWithDefault default' l' acc))
           False = undefined
      go (Edge l l':xs) acc =
        if not $ in'new `lessThan` in'old
          then let workList' = ControlFlowGraph.withStart l' controlFlow ++ xs
                   accumulator = Map.insert l' (in'new `union` in'old) acc
                in go workList' accumulator
          else go xs acc
        where
          target = controlFlowGraph ! l
          in'new = function target (Map.findWithDefault default' l acc)
          in'old = Map.findWithDefault default' l' acc
   in Result
        { result =
            Map.mapWithKey
              (\key value -> function (controlFlowGraph ! key) value)
              (go workList initial)
        }
  where
    controlFlow = flow givenData
    workList = Set.toList controlFlow
    function = transferFunction givenData
    default' = bottom givenData
    controlFlowGraph = graph givenData
    fins = finals givenData
    initial =
      Map.fromList $
      zip fins [function (controlFlowGraph ! n) default' | n <- fins]

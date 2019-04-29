module Main where

import AbstractSyntaxTree
import ControlFlowGraph
import DFAFramework
import Data.Set as Set
import Data.String
import Prelude hiding (all)
import Variables
import Data.Map ((!))
import TransferFunction
import LiveVariables

program :: Program
program =
  Program
    (Compose
       (Compose
          (Assign (Id "a") (Number 10) (Label 1))
          (While
             (Var (Id "a"))
             (If
                (Minus (Var (Id "a")) (Number 5))
                (Assign (Id "a") (Number 0) (Label 4))
                (Assign (Id "a") (Minus (Var (Id "a")) (Number 1)) (Label 5))
                (Label 3))
             (Label 2)))
       (Print (Var (Id "a")) (Label 6)))


main :: IO ()
main = do
  print $ (mkGraph program ! (Label 1))
  print $ solve problem
  where
    problem = DFA {
      bottom = Set.empty,
      flow = nodesFromProgram program,
      graph = mkGraph program,
      transferFunction = mkTransferFunction LiveVariables.kill LiveVariables.gen Set.empty
}
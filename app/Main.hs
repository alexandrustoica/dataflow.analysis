module Main where

import           AbstractSyntaxTree
import           ControlFlowGraph

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
  print $ nodesFromProgram program

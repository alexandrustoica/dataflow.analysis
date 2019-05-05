module Main where

import           AbstractSyntaxTree
import           ControlFlowGraph   hiding (finals)
import           Data.Map           ((!))
import           Data.Set           as Set
import           Data.String
import           LiveVariables
import           Prelude            hiding (all)
import           Variables

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

other :: Program
other =
  Program
    (Compose
       (Compose
          (Assign (Id "a") (Number 0) (Label 1))
          (While
             (Minus (Var (Id "a")) (Number 5))
             (Compose
                (Assign (Id "b") (Plus (Var (Id "a")) (Number 1)) (Label 3))
                (Compose
                   (Assign
                      (Id "c")
                      (Plus (Var (Id "c")) (Var (Id "b")))
                      (Label 4))
                   (Assign
                      (Id "a")
                      (Multiply (Var (Id "b")) (Number 2))
                      (Label 5))))
             (Label 2)))
       (Print (Var (Id ("c"))) (Label 6)))

main :: IO ()
main = print $ LiveVariables.analyse other

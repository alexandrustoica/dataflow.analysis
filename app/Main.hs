module Main where

import Program (
  Expression (..),
  Id (..),
  Program (..),
  Statement (..))


main :: IO ()
main =
  print $
  Program
    (Composition
      (Assign (Id "x") (Number 2))
      (If (Variable (Id "x"))
        (Print (Number 7))
        (While (Plus (Variable (Id "x")) (Number 1))
          (Composition
            (Print (Variable (Id "x")))
            (Assign (Id "x")
              (Minus (Variable (Id "x")) (Number 1)))))))
module Main where

import           Program
import           ToCFGraph (fromProgram)

program :: Program
program =
  Program
    (Composition
       (Composition
          (Assign (Id "a") (Number 10))
          (While
             (Variable (Id "a"))
             (Composition
                (Assign (Id "a") (Minus (Variable (Id "a")) (Number 1)))
                (If
                   (Minus (Variable (Id "a")) (Number 5))
                   (Assign (Id "a") (Number 0))
                   (Assign (Id "a") (Minus (Variable (Id "a")) (Number 1)))))))
       (Print (Variable (Id "a"))))

main :: IO ()
main = do
  print $ fromProgram (Program (Print (Variable (Id "a"))))
  print $ fromProgram program

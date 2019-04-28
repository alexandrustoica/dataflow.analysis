module AbstractSyntaxTree
  ( Program(..)
  , Statement(..)
  , Expression(..)
  , Id(..)
  , Label(..)
  , left
  , right
  ) where

import qualified Data.Map.Strict as Map

newtype Program =
  Program Statement
  deriving (Eq)

data Statement
  = Compose Statement Statement
  | Print Expression Label
  | Assign Id Expression Label
  | If Expression Statement Statement Label
  | While Expression Statement Label
  deriving (Eq)

data Expression
  = Var Id
  | Number Int
  | Plus Expression Expression
  | Minus Expression Expression
  | Multiply Expression Expression
  | Divide Expression Expression
  deriving (Eq)

left :: Expression -> Expression
left (Plus lhs rhs) = lhs
left (Multiply lhs rhs) = lhs
left (Minus lhs rhs) = lhs
left (Divide lhs rhs) = lhs

right :: Expression -> Expression
right (Plus lhs rhs) = rhs
right (Multiply lhs rhs) = rhs
right (Minus lhs rhs) = rhs
right (Divide lhs rhs) = rhs


newtype Label =
  Label Int
  deriving (Show, Eq, Ord)

newtype Id =
  Id String
  deriving (Eq, Ord)

instance Show Program where
  show (Program program) = show program

instance Show Statement where
  show (Compose a b) = show a ++ "; " ++ show b
  show (Print expr _) = "print " ++ show expr
  show (Assign id value _) = show id ++ " := " ++ show value
  show (If cond true false _) =
    "if " ++ show cond ++ " then " ++ show true ++ " else " ++ show false
  show (While cond body _) = "while " ++ show cond ++ " " ++ show body

instance Show Id where
  show (Id value) = show value

instance Show Expression where
  show (Number value) = show value
  show (Var variable) = show variable
  show expression = "(" ++ go' expression ++ ")"
    where
      go' (Plus lhs rhs)     = show lhs ++ " + " ++ show rhs
      go' (Minus lhs rhs)    = show lhs ++ " - " ++ show rhs
      go' (Multiply lhs rhs) = show lhs ++ " * " ++ show rhs
      go' (Divide lhs rhs)   = show lhs ++ " \\ " ++ show rhs

module Program
  ( Program(..)
  , Statement(..)
  , Expression(..)
  , Id(..)
  ) where


newtype Program =
  Program Statement
  deriving (Eq)

data Statement
  = Composition Statement Statement
  | Print Expression
  | Assign Id Expression
  | If Expression Statement Statement
  | While Expression Statement
  deriving Eq

data Expression
  = Variable Id
  | Number Int
  | Plus Expression Expression
  | Minus Expression Expression
  | Multiply Expression Expression
  | Divide Expression Expression
  deriving Eq

newtype Id =
  Id String
  deriving Eq

instance Show Program where
  show (Program program) = show program

instance Show Statement where
  show (Composition a b) = show a ++ "; " ++ show b
  show (Print expr) = "print " ++ show expr
  show (Assign id value) = show id ++ " := " ++ show value
  show (If cond true false) = "if " ++ show cond
    ++ " then " ++ show true ++ " else " ++ show false
  show (While cond body) = "while " ++ show cond ++ " " ++ show body

instance Show Id where
  show (Id value) = show value

instance Show Expression where
  show (Number value) = show value
  show (Variable variable) = show variable
  show expression = "(" ++ go' expression ++ ")" where
    go' (Plus lhs rhs) = show lhs ++ " + " ++ show rhs
    go' (Minus lhs rhs) = show lhs ++ " - " ++ show rhs
    go' (Multiply lhs rhs) = show lhs ++ " * " ++ show rhs
    go' (Divide lhs rhs) = show lhs ++ " \\ " ++ show rhs
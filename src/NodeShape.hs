module NodeShape
  ( Shape(..)
  ) where

import           Program (Expression (..), Id (..))

data Shape
  = NAssign Id Expression
  | NPrint Expression
  | NExpression Expression
  deriving (Show, Eq)

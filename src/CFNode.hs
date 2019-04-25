module CFNode
  ( Node(..)
  , NodeId(..)
  , Shape(..)
  , empty
  ) where

import           Program (Expression (..), Id (..))

data Shape
  = NAssign Id Expression
  | NPrint Expression
  | NExpression Expression
  deriving (Show, Eq)

newtype NodeId =
  NId Int
  deriving (Show, Eq, Ord)

data Node =
  Node NodeId Shape
  deriving (Show)

instance Eq Node where
  (Node lhs _) == (Node rhs _) = lhs == rhs

instance Ord Node where
  (Node lhs _) <= (Node rhs _) = lhs <= rhs

empty :: Node
empty = Node (NId 0) (NExpression (Number 0))

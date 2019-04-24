module Node
  ( NodeId(..)
  , Node(..)
  , Shape(..)
  ) where

import           NodeShape (Shape (..))

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

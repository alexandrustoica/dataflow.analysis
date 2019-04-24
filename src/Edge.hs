module Edge where

import Node (Node (..))

data Edge =
  Edge Node Node
  deriving (Show, Eq, Ord)

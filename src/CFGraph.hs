module CFGraph
  ( Graph(..)
  , Edge(..)
  , mkGraph
  , startNode
  , pred
  , next
  , add
  ) where

import qualified Data.Map.Strict as Map
import           Prelude         hiding (pred)
import           AbstractSyntaxTree         (Expression (..), Id (..))

newtype Node = Node Int deriving (Show, Eq, Ord)

data Edge =
  Edge Node Node
  deriving (Show, Eq, Ord)

data Graph =
  Graph Node (Map.Map Node [Node]) (Map.Map Node [Node])
  deriving (Show)

mkGraph :: Node -> Graph
mkGraph n = Graph n Map.empty Map.empty

startNode :: Graph -> Node
startNode (Graph n _ _) = n

next :: Node -> Graph -> [Node]
next n (Graph _ suc _) = Map.findWithDefault [] n suc

pred :: Node -> Graph -> [Node]
pred n (Graph _ _ pred) = Map.findWithDefault [] n pred

add :: Graph -> Edge -> Graph
add g@(Graph x suc pred) (Edge s p) =
  Graph x (Map.insertWith (++) p [s] suc) (Map.insertWith (++) s [p] pred)

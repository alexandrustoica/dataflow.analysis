module ControlFlowGraph
  ( Graph(..)
  , startNode
  , mkGraph
  ) where

import qualified Data.Map.Strict as Map

import           Edge            (Edge (..))
import           Node            (Node (..))

data Graph =
  Graph Node (Map.Map Node [Node]) (Map.Map Node [Node])
  deriving (Show)

class (Show g) => CFGraph g where
  pred :: Node -> g -> [Node]
  next :: Node -> g -> [Node]
  add :: g -> Edge -> g

mkGraph :: Node -> Graph
mkGraph n = Graph n Map.empty Map.empty

startNode :: Graph -> Node
startNode (Graph n _ _) = n

instance CFGraph Graph where
  next n (Graph _ suc _) = Map.findWithDefault [] n suc
  pred n (Graph _ _ pred) = Map.findWithDefault [] n pred
  add g@(Graph x suc pred) (Edge s p) =
    Graph x (Map.insertWith (++) p [s] suc) (Map.insertWith (++) s [p] pred)

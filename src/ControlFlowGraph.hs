module ControlFlowGraph
  ( Graph(..)
  , NodeId(..)
  , Node(..)
  , start
  , controlFlowGraphFrom
  ) where

import qualified Data.Map.Strict as Map
import Program (Id(..), Expression(..), Statement(..), Program(..))

newtype NodeId = NId Int deriving (Show, Eq)

data Shape
  = NAssign Id Expression
  | NPrint Expression
  | NExpression Expression deriving (Show, Eq)

data Node = UnitNode | Node NodeId Shape deriving (Show)

data Graph = Graph
  Node
  (Map.Map Node [Node])
  (Map.Map Node [Node]) deriving (Show)


start :: Graph -> Node
start (Graph start _ _) = start

emptyGraph :: Graph
emptyGraph = Graph UnitNode Map.empty Map.empty

controlFlowGraphFrom :: Program -> Graph
controlFlowGraphFrom program = emptyGraph
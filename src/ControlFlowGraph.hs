module ControlFlowGraph where

import           AbstractSyntaxTree
import           Data.Set           as Set

data CFNode
  = In Label Label
  | Out Label Label
  deriving (Eq, Ord)

instance Show CFNode where
  show (In a b)  = show a ++ " -> " ++ show b
  show (Out a b) = show a ++ " <- " ++ show b

-- Returns the initial nodes labels in our control flow graph
initial :: Statement -> Label
initial (Compose lhs rhs) = initial lhs
initial (Print _ l)       = l
initial (Assign _ _ l)    = l
initial (While _ _ l)     = l
initial (If _ _ _ l)      = l

-- Returns the final nodes labels in our control flow graph
finals :: Statement -> Set Label
finals (Print _ l)      = Set.singleton l
finals (Assign _ _ l)   = Set.singleton l
finals (If _ lhs rhs _) = finals lhs `union` finals rhs
finals (While _ _ l)    = Set.singleton l
finals (Compose _ rhs)  = finals rhs

initialFromProgram :: Program -> Label
initialFromProgram (Program s) = initial s

finalsFromProgram :: Program -> Set Label
finalsFromProgram (Program s) = finals s

nodes :: Statement -> Set CFNode
nodes (Compose lhs rhs) =
  nodes lhs `union` nodes rhs `union`
  Set.fromList [In x $ initial rhs | x <- Set.toList $ finals lhs]
nodes (If _ lhs rhs l) =
  nodes lhs `union` nodes rhs `union`
  Set.fromList [In l $ initial lhs, In l $ initial rhs]
nodes (While _ statement l) =
  nodes statement `union` Set.singleton (In l $ initial statement) `union`
  Set.fromList [In f l | f <- Set.toList $ finals statement]
nodes _ = Set.empty

nodesFromProgram :: Program -> Set CFNode
nodesFromProgram (Program s) = nodes s

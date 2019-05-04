module ControlFlowGraph
  ( Edge(..)
  , CFGraph(..)
  , initial
  , withStart
  , finals
  , initialFromProgram
  , finalsFromProgram
  , nodesFromProgram
  , labelsIn
  , mkGraph
  ) where

import AbstractSyntaxTree
import Data.Set as Set
import qualified Data.Map.Strict as Map

data Edge
  = Edge Label Label
  deriving (Eq, Ord)

instance Show Edge where
  show (Edge a b) = show a ++ " -> " ++ show b

type CFGraph = Map.Map Label Statement

withStart :: Label -> Set Edge -> [Edge]
withStart l set = Set.toList $ Set.filter predicate set
  where predicate (Edge lhs _) = lhs == l

labelsFor :: Statement -> [(Label, Statement)]
labelsFor s@(Print _ l) = [(l, s)]
labelsFor s@(Assign _ _ l) = [(l, s)]
labelsFor s@(If _ lhs rhs l) =
  (l, s) : labelsFor lhs ++ labelsFor rhs
labelsFor s@(While _ statement l) =
  (l, s) : labelsFor statement
labelsFor (Compose lhs rhs) =
  labelsFor lhs ++ labelsFor rhs

labelsIn :: Program -> [(Label, Statement)]
labelsIn (Program s) = labelsFor s

mkGraph :: Program -> CFGraph
mkGraph p = Map.fromList $ labelsIn p

-- Returns the initial nodes labels in our control flow graph
initial :: Statement -> Label
initial (Compose lhs rhs) = initial lhs
initial (Print _ l) = l
initial (Assign _ _ l) = l
initial (While _ _ l) = l
initial (If _ _ _ l) = l

-- Returns the final nodes labels in our control flow graph
finals :: Statement -> Set Label
finals (Print _ l) = Set.singleton l
finals (Assign _ _ l) = Set.singleton l
finals (If _ lhs rhs _) = finals lhs `union` finals rhs
finals (While _ _ l) = Set.singleton l
finals (Compose _ rhs) = finals rhs

initialFromProgram :: Program -> Label
initialFromProgram (Program s) = initial s

finalsFromProgram :: Program -> Set Label
finalsFromProgram (Program s) = finals s

nodes :: Statement -> Set Edge
nodes (Compose lhs rhs) =
  nodes lhs `union` nodes rhs `union`
  Set.fromList [Edge x $ initial rhs | x <- Set.toList $ finals lhs]
nodes (If _ lhs rhs l) =
  nodes lhs `union` nodes rhs `union`
  Set.fromList [Edge l $ initial lhs, Edge l $ initial rhs]
nodes (While _ statement l) =
  nodes statement `union` Set.singleton (Edge l $ initial statement) `union`
  Set.fromList [Edge f l | f <- Set.toList $ finals statement]
nodes _ = Set.empty

reverseNodes :: Set Edge -> Set Edge
reverseNodes s = Set.fromList [go x | x <- Set.toList s]
  where go (Edge lhs rhs) = Edge rhs lhs

nodesFromProgram :: Program -> Set Edge
nodesFromProgram (Program s) = nodes s
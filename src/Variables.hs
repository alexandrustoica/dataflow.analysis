module Variables
  ( all
  ) where

import           AbstractSyntaxTree
import           Data.Set           as Set
import           Prelude            hiding (all)

allFromExpression :: Expression -> Set Id
allFromExpression (Number _) = Set.empty
allFromExpression (Var x) = Set.singleton x
allFromExpression expr = go (left expr) (right expr)
  where
    go x y = allFromExpression x `union` allFromExpression y

allFromStatement :: Statement -> Set Id
allFromStatement (Print expr _) = allFromExpression expr
allFromStatement (Assign id expr _) = allFromExpression expr
allFromStatement (Compose lhs rhs) =
  allFromStatement lhs `union` allFromStatement rhs
allFromStatement (If expr lhs rhs _) =
  allFromExpression expr `union` allFromStatement lhs `union`
  allFromStatement rhs
allFromStatement (While expr statement _) =
  allFromExpression expr `union` allFromStatement statement

all :: Program -> Set Id
all (Program s) = allFromStatement s

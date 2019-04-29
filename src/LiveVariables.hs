module LiveVariables where

import AbstractSyntaxTree
import Data.Set as Set
import Variables

kill :: Statement -> Set Id -> Set Id
kill (Assign id _ _) _ = Set.singleton id
kill _ _ = Set.empty

gen :: Statement -> Set Id
gen (Print expr _) = Variables.allFromExpression expr
gen (While expr _ _) = Variables.allFromExpression expr
gen (If expr _ _ _) = Variables.allFromExpression expr
gen (Assign _ expr _) = Variables.allFromExpression expr
gen (Compose lhs rhs) = Set.empty
module LiveVariables where

import           AbstractSyntaxTree
import           ControlFlowGraph   hiding (finals)
import           Data.Set           as Set
import           Debug.Trace
import           DFAFramework
import           TransferFunction
import           Variables

def :: Statement -> Set Id -> Set Id
def (Assign id _ _) _ = Set.singleton id
def _ _ = Set.empty

use :: Statement -> Set Id
use (Assign _ expr _) = Variables.allFromExpression expr
use (Print expr _)    = Variables.allFromExpression expr
use (While expr _ _)  = Variables.allFromExpression expr
use (If expr _ _ _)   = Variables.allFromExpression expr
use _                 = Set.empty

analyse :: Program -> Result (Set Id)
analyse program = backward problem
  where
    problem =
      DFA
        { bottom = Set.empty
        , flow = reverse' $ edgesFrom program
        , graph = mkGraph program
        , finals = Set.toList $ finalsFromProgram program
        , program = program
        , transferFunction = mkTransferFunction def use Set.empty
        }

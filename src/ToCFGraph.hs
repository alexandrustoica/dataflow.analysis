module ToCFGraph
  ( fromProgram
  ) where

import           CFGraph (Graph (..), mkGraph)
import           CFNode  (Node (..), NodeId (..), Shape (..), empty)
import           Program (Expression (..), Program (..), Statement (..))

fromExpression :: Expression -> Node
fromExpression expr = Node (NId 0) (NExpression expr)

fromStatement :: Statement -> Graph -> Graph
fromStatement (Print expr) g = mkGraph $ fromExpression expr

fromProgram :: Program -> Graph
fromProgram (Program s) = fromStatement s $ mkGraph empty

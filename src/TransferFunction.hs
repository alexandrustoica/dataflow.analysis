module TransferFunction
  ( mkTransferFunction
  ) where

import           AbstractSyntaxTree
import           Lattice
import           Variables

mkTransferFunction ::
     (Lattice a)
  => (Statement -> a -> a)
  -> (Statement -> a)
  -> a
  -> (Statement -> a -> a)
mkTransferFunction kill gen bottom statement out =
  gen statement `union` (out `difference` kill statement bottom)


module Optimization.Optimization where


import qualified Data.Map as Map
import Control.Monad.State

import Typechecker
import Optimization.Folding

optimize :: [CFunction] -> [CFunction]
optimize fs = 
  let (val, state) = runState (optimizeFold fs) (Map.empty, 0)
  in undefined
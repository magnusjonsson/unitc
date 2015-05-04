module Unit where

import Prelude hiding (div)
import Data.Ratio
import Data.Map.Strict as Map

type Unit = Map String R
type R = Ratio Integer

nonzero :: R -> Maybe R
nonzero 0 = Nothing
nonzero x = Just x

one :: Unit
one = Map.empty

fundamental :: String -> Unit
fundamental name = Map.singleton name 1

mul ::  Unit -> Unit -> Unit
mul = Map.mergeWithKey (\k p1 p2 -> nonzero (p1 + p2)) id id

recip :: Unit -> Unit
recip = Map.map negate
            
div ::  Unit -> Unit -> Unit
div = Map.mergeWithKey (\k p1 p2 -> nonzero (p1 - p2)) id id

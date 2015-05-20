module App.Unit where

import Prelude hiding (div, recip)
import Data.Ratio
import Data.Map.Strict as Map

type Unit = Map String Q
type Q = Ratio Integer

nonzero :: Q -> Maybe Q
nonzero 0 = Nothing
nonzero x = Just x

one :: Unit
one = Map.empty

fundamental :: String -> Unit
fundamental name = Map.singleton name 1

mul ::  Unit -> Unit -> Unit
mul = Map.mergeWithKey (\_k p1 p2 -> nonzero (p1 + p2)) id id

recip :: Unit -> Unit
recip = Map.map negate
            
div ::  Unit -> Unit -> Unit
div = Map.mergeWithKey (\_k p1 p2 -> nonzero (p1 - p2)) id recip

pow :: Unit -> Q -> Unit
pow u q = Map.map (q *) u

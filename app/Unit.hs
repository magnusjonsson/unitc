module Unit where

import Prelude hiding (div, recip)
import Data.Ratio
import Data.List as List
import Data.Map.Strict as Map

type Q = Ratio Integer

newtype Unit = Unit (Map String Q)
        deriving (Eq)

nonzero :: Q -> Maybe Q
nonzero 0 = Nothing
nonzero x = Just x

one :: Unit
one = Unit Map.empty

fundamental :: String -> Unit
fundamental name = Unit (Map.singleton name 1)

mul ::  Unit -> Unit -> Unit
mul (Unit u1) (Unit u2) = Unit (Map.mergeWithKey (\_k p1 p2 -> nonzero (p1 + p2)) id id u1 u2)

recip :: Unit -> Unit
recip (Unit u) = Unit (Map.map negate u)
            
div ::  Unit -> Unit -> Unit
div (Unit u1) (Unit u2) = Unit (Map.mergeWithKey (\_k p1 p2 -> nonzero (p1 - p2)) id (Map.map negate) u1 u2)

pow :: Unit -> Q -> Unit
pow (Unit u) q = Unit (Map.map (q *) u)


instance Show Unit where
  show (Unit u) =
    let
      (positive, negative) = List.partition (\(k,p) -> p >= 0) (Map.toList u)
    in
      (if List.null positive then
         "1"
       else
         intercalate " * " (List.map (\ (k, p) -> showPower k p) positive)
      ) ++
      List.concatMap (\ (k, p) -> " / " ++ showPower k (-p)) negative


showPower :: String -> Q -> String
showPower name power =
  case (numerator power, denominator power) of
    (1, 1) -> name
    (n, 1) -> name ++ "**" ++ show n
    (n, d) -> name ++ "**(" ++ show n ++ "/" ++ show d ++ ")"

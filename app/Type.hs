module Type where

import Prelude hiding (and, or, div, abs, min, max)

import qualified Unit
import Control.Monad
import Data.Maybe (isJust)

data Type = Numeric (Maybe Unit.Unit) -- Must be (Just _) once fully formed
          | Fun Type [Type] Bool -- returnType names acceptsVarArgs
          | Struct String
          | Void
          | Any -- Wildcard, useful to bypass type checking for some builtins
          | Zero
          | Ptr Type
          | Arr Type
          | VaList
    deriving (Show, Eq)

type Unit = Unit.Unit

one :: Type
one = Numeric (Just Unit.one)

add :: Type -> Type -> Maybe Type
add x Zero = add x one
add Zero x = add one x
add (Numeric (Just t1)) (Numeric (Just t2)) | t1 == t2 = Just (Numeric (Just t1))
--add (Ptr Void) (Numeric _) = Nothing -- disallowed by the standard, but allowed by gcc
--add (Numeric _) (Ptr Void) = Nothing -- disallowed by the standard, but allowed by gcc
add (Arr t) (Numeric u) = if u == Just Unit.one then Just (Ptr t) else Nothing
add (Ptr t) (Numeric u) = if u == Just Unit.one then Just (Ptr t) else Nothing
add (Numeric u) (Arr t) = if u == Just Unit.one then Just (Ptr t) else Nothing
add (Numeric u) (Ptr t) = if u == Just Unit.one then Just (Ptr t) else Nothing
add _ _ = Nothing

applyMathFn :: (Unit -> Unit) -> Type -> Maybe Type
applyMathFn fn t =
  do unit <- case t of
               Zero -> Just Unit.one
               Numeric (Just unit) -> Just unit
               _ -> Nothing
     return (Numeric (Just (fn unit)))

pow :: Type -> Unit.Q -> Maybe Type
pow ty power = applyMathFn (`Unit.pow` power) ty

sqrt :: Type -> Maybe Type
sqrt ty = applyMathFn Unit.sqrt ty

sub :: Type -> Type -> Maybe Type
sub x Zero = sub x one
sub Zero x = sub one x
sub (Numeric (Just t1)) (Numeric (Just t2)) | t1 == t2 = Just (Numeric (Just t1))
--sub (Ptr Void) (Numeric _) = Nothing -- disallowed by the standard, but allwed by gcc
sub (Arr t) (Numeric u) = if u == Just Unit.one then Just (Ptr t) else Nothing
sub (Ptr t) (Numeric u) = if u == Just Unit.one then Just (Ptr t) else Nothing
sub (Ptr t1) (Ptr t2) = if t1 == t2 then Just one else Nothing
sub _ _ = Nothing

mul :: Type -> Type -> Maybe Type
mul x Zero = mul x one
mul Zero x = mul one x
mul (Numeric (Just t1)) (Numeric (Just t2)) = Just (Numeric (Just (Unit.mul t1 t2)))
mul _ _ = Nothing

div :: Type -> Type -> Maybe Type
div x Zero = div x one
div Zero x = div one x
div (Numeric (Just t1)) (Numeric (Just t2)) = Just (Numeric (Just (Unit.div t1 t2)))
div _ _ = Nothing

rem :: Type -> Type -> Maybe Type
rem = sub

shl :: Type -> Type -> Maybe Type
shl t1 t2 =
  if numeric t1 then
    case t2 of
      Numeric (Just u) ->
       if u == Unit.one then
         Just t1
       else
         Nothing
      _ -> Nothing
  else
    Nothing

shr :: Type -> Type -> Maybe Type
shr = shl

and :: Type -> Type -> Maybe Type
and = add

or :: Type -> Type -> Maybe Type
or = and

xor :: Type -> Type -> Maybe Type
xor = or

land :: Type -> Type -> Maybe Type
land t1 t2 =
  if booleanable t1 && booleanable t2 then
    Just one
  else
    Nothing

lor :: Type -> Type -> Maybe Type
lor = land

cmp :: Type -> Type -> Maybe Type
cmp t1 t2 =
  case merge t1 t2 of
    Nothing -> Nothing
    Just t -> Just one

comp :: Type -> Maybe Type
comp t =
  case t of
    Zero -> Just one
    Numeric (Just u) | u == Unit.one -> Just one
    _ -> Nothing

neg :: Type -> Maybe Type
neg t =
  case t of
    Zero -> Just one
    Numeric _ -> Just one
    Ptr _ -> Just one
    Arr _ -> Just one
    _ -> Nothing

abs :: Type -> Maybe Type
abs = applyMathFn id

min :: Type -> Type -> Maybe Type
min t1 t2 =
  case (t1, t2) of
    (Zero, Zero) -> Just one
    (Zero, Numeric u) -> Just t2
    (Numeric u, Zero) -> Just t1
    (Numeric u1, Numeric u2) | u1 == u2 -> Just t1
    _ -> Nothing

max :: Type -> Type -> Maybe Type
max = min

assignable :: Type -> Type -> Bool
assignable to from =
  case (to, from) of
    (Zero, Zero) -> True
    (Zero, _) -> False
    (Ptr (Fun _ _ _), Fun _ _ _) -> assignable to (Ptr from)
    (Ptr Void, Arr from') -> True
    (Ptr to', Arr from') -> isJust (merge to' from')
    _ -> isJust (merge to from)

booleanable :: Type -> Bool
booleanable t =
  case t of
    Numeric _ -> True
    Fun _ _ _ -> True
    Struct _ -> False
    Void -> False
    Any -> True
    Zero -> True
    Ptr _ -> True
    Arr _ -> True
    VaList -> False

deref :: Type -> Maybe Type
deref t =
  case t of
    Ptr t' -> Just t'
    Arr t' -> Just t'
    Fun _ _ _ -> return t
    _ -> Nothing

numeric :: Type -> Bool
numeric t =
  case t of
    Zero -> True
    Numeric _ -> True
    _ -> False

merge :: Type -> Type -> Maybe Type
merge t1 t2 =
    case (t1, t2) of
      (Any, _) -> Just t2
      (_, Any) -> Just t1
      (Void, Void) -> Just Void
      (Zero, Zero) -> Just Zero
      (Ptr t, Zero) -> Just t1
      (Zero, Ptr t) -> Just t2
      (Ptr Void, Ptr _) -> Just t2
      (Ptr _, Ptr Void) -> Just t1
      (Numeric Nothing, Zero) -> Just t1
      (Zero, Numeric Nothing) -> Just t2
      (Numeric (Just unit), Zero) -> Just t1
      (Zero, Numeric (Just unit)) -> Just t2
      (Numeric m1, Numeric m2) ->
          case (m1, m2) of
            (Just u1, Just u2) -> if u1 == u2 then Just (Numeric (Just u1)) else Nothing
            (Just u1, Nothing) -> Just (Numeric (Just u1))
            (Nothing, Just u2) -> Just (Numeric (Just u2))
            (Nothing, Nothing) -> Just (Numeric Nothing)
      (Fun r1 a1 d1, Fun r2 a2 d2) ->
          -- maybe monad
          do r <- merge r1 r2
             a <- mapM (uncurry merge) (zip a1 a2)
             guard (d1 == d2)
             return (Fun r a d1)
      (Struct n1, Struct n2) -> if n1 == n2 then Just t1 else Nothing
      (VaList, VaList) -> Just VaList
      (Ptr t1', Ptr t2') -> do t' <- merge t1' t2'; return (Ptr t')
      (Arr t1', Arr t2') -> do t' <- merge t1' t2'; return (Arr t')
      _ -> Nothing

mergeMaybe :: Maybe Type -> Maybe Type -> Maybe Type
mergeMaybe m1 m2 =
    case (m1, m2) of
      (Just t1, Just t2) -> merge t1 t2
      (Just t1, Nothing) -> Just t1
      (Nothing, Just t2) -> Just t2
      (Nothing, Nothing) -> Nothing

monomorphize :: Type -> Type
monomorphize t =
    case t of
      Numeric Nothing -> Numeric (Just Unit.one) -- polymorphic unit becomes unit 1
      Numeric (Just u) -> t
      Void -> Void
      Zero -> one
      Struct _ -> t
      VaList -> VaList
      Any -> error "monomorphize any!"
      Ptr t' -> Ptr (monomorphize t')
      Arr t' -> Arr (monomorphize t')
      Fun r a v -> Fun (monomorphize r) (map monomorphize a) v

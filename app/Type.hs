module Type where

import Prelude hiding (and, or, div)

import qualified Unit
import Control.Monad

data Type = Numeric (Maybe Unit.Unit) -- Must be (Just _) once fully formed
          | Fun Type [(Maybe String, Type)] Bool -- returnType argNamesAndTypes acceptsVarArgs
          | Struct String
          | Void
          | Any -- Wildcard, useful to bypass type checking for some builtins
          | Zero
          | Ptr Type
          | Arr Type
          | VaList
    deriving (Show, Eq)


one :: Type
one = Numeric (Just Unit.one)

add :: Type -> Type -> Maybe Type
add x Zero = add x one
add Zero x = add one x
add (Numeric (Just t1)) (Numeric (Just t2)) | t1 == t2 = Just (Numeric (Just t1))
add (Ptr Void) (Numeric _) = Nothing
add (Numeric _) (Ptr Void) = Nothing
add (Ptr t) (Numeric u) = if u == Just Unit.one then Just (Ptr t) else Nothing
add (Numeric u) (Ptr t) = if u == Just Unit.one then Just (Ptr t) else Nothing
add _ _ = Nothing


sub :: Type -> Type -> Maybe Type
sub x Zero = sub x one
sub Zero x = sub one x
sub (Numeric (Just t1)) (Numeric (Just t2)) | t1 == t2 = Just (Numeric (Just t1))
sub (Ptr Void) (Numeric _) = Nothing
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

land :: Type -> Type -> Maybe Type
land = and

lor :: Type -> Type -> Maybe Type
lor = or

xor :: Type -> Type -> Maybe Type
xor = or

cmp :: Type -> Type -> Maybe Type
cmp t1 t2 =
  case merge t1 t2 of
    Nothing -> Nothing
    Just t -> Just one

assignable :: Type -> Type -> Bool
assignable to from =
    case (to, from) of
    -- Some special cases
    -- Generic merge
    -- TODO do this better
    _ -> case merge to from of
          Nothing -> False
          Just _ -> True

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
      (Ptr Void, Zero) -> Just (Ptr Void)
      (Ptr Void, Ptr _) -> Just t2
      (Ptr _, Ptr Void) -> Just t1
      (Numeric Nothing, Zero) -> Just t1
      (Zero, Numeric Nothing) -> Just t2
      (Numeric (Just unit), Zero) | unit == Unit.one -> Just t1
      (Zero, Numeric (Just unit)) | unit == Unit.one -> Just t2
      (Numeric m1, Numeric m2) ->
          case (m1, m2) of
            (Just u1, Just u2) -> if u1 == u2 then Just (Numeric (Just u1)) else Nothing
            (Just u1, Nothing) -> Just (Numeric (Just u1))
            (Nothing, Just u2) -> Just (Numeric (Just u2))
            (Nothing, Nothing) -> Just (Numeric Nothing)
      (Fun r1 a1 d1, Fun r2 a2 d2) ->
          -- maybe monad
          do r <- merge r1 r2
             a <- mapM (uncurry mergeArg) (zip a1 a2)
             guard (d1 == d2)
             return (Fun r a d1)
      (Struct n1, Struct n2) -> if n1 == n2 then Just t1 else Nothing
      (VaList, VaList) -> Just VaList
      (Ptr t1', Ptr t2') -> do t' <- merge t1' t2'; return (Ptr t')
      (Arr t1', Arr t2') -> do t' <- merge t1' t2'; return (Arr t')
      _ -> Nothing

mergeArg :: (Maybe String, Type) -> (Maybe String, Type) -> Maybe (Maybe String, Type)
mergeArg (name1, ty1) (name2, ty2) =
  do ty <- merge ty1 ty2
     return (name1 `mplus` name2, ty)

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
      _ -> t

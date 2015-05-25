module App.Type where

import App.Unit as Unit
import Control.Monad

data Type = Numeric (Maybe Unit)
          | Fun Type [Type] Bool -- returnType argTypes acceptsVarArgs
          | Struct String
          | Other
    deriving (Show, Eq)

add :: Type -> Type -> Maybe Type
add (Numeric (Just t1)) (Numeric (Just t2)) | t1 == t2 = Just (Numeric (Just t1))
add (Numeric Nothing) (Numeric Nothing) = Just (Numeric Nothing)
add _ _ = Nothing

sub :: Type -> Type -> Maybe Type
sub (Numeric (Just t1)) (Numeric (Just t2)) | t1 == t2 = Just (Numeric (Just t1))
sub (Numeric Nothing) (Numeric Nothing) = Just (Numeric Nothing)
sub _ _ = Nothing

mul :: Type -> Type -> Maybe Type
mul (Numeric (Just t1)) (Numeric (Just t2)) = Just (Numeric (Just (Unit.mul t1 t2)))
mul (Numeric Nothing) (Numeric Nothing) = Just (Numeric Nothing)
mul _ _ = Nothing

div :: Type -> Type -> Maybe Type
div (Numeric (Just t1)) (Numeric (Just t2)) = Just (Numeric (Just (Unit.div t1 t2)))
div (Numeric Nothing) (Numeric Nothing) = Just (Numeric Nothing)
div _ _ = Nothing

merge :: Type -> Type -> Maybe Type
merge t1 t2 =
    case (t1, t2) of
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
      (Other, Other) -> Just Other
      _ -> Nothing

mergeMaybe :: Maybe Type -> Maybe Type -> Maybe Type
mergeMaybe m1 m2 =
    case (m1, m2) of
      (Just t1, Just t2) -> merge t1 t2
      (Just t1, Nothing) -> Just t1
      (Nothing, Just t2) -> Just t2
      (Nothing, Nothing) -> Nothing

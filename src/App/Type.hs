module App.Type where

import App.Unit as Unit

data Type = Numeric (Maybe Unit)
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

{-# LANGUAGE FlexibleInstances #-}

module FindUnit where

import Unit
import Monad.Analysis
import Control.Monad
import Control.Monad.Extra
import Language.C.Pretty
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Syntax.Constants
import Language.C.Syntax.AST

class FindUnit a where
    findUnit :: a -> Analysis (Maybe Unit)

instance FindUnit Unit where
    findUnit u = return (Just u)

instance FindUnit CDeclr where
    findUnit declr =
        case declr of
          CDeclr _ _ _ attrs _ -> findUnit attrs

instance FindUnit CAttr where
    findUnit attr =
        case attr of
          CAttr (Ident "unit" _ _) [e] _ -> parseCExprAsUnit e
          _ -> return Nothing

parseCExprAsQ :: CExpr -> Analysis (Maybe Q)
parseCExprAsQ expr =
    case expr of
      CConst (CIntConst (CInteger i _ _) _) -> return (Just (toRational i))
      CUnary CNegOp e _ ->
          do u <- parseCExprAsQ e
             return (liftM negate u)
      CBinary CAddOp e1 e2 _ ->
          do u1 <- parseCExprAsQ e1
             u2 <- parseCExprAsQ e2
             return (liftM2 (+) u1 u2)
      CBinary CSubOp e1 e2 _ ->
          do u1 <- parseCExprAsQ e1
             u2 <- parseCExprAsQ e2
             return (liftM2 (-) u1 u2)
      CBinary CMulOp e1 e2 _ ->
          do u1 <- parseCExprAsQ e1
             u2 <- parseCExprAsQ e2
             return (liftM2 (*) u1 u2)
      CBinary CDivOp e1 e2 _ ->
          do u1 <- parseCExprAsQ e1
             u2 <- parseCExprAsQ e2
             return (liftM2 (/) u1 u2)
      _ -> do err expr ("Can't parse expression as power: " ++ show (pretty expr))
              return Nothing

parseCExprAsUnit :: CExpr -> Analysis (Maybe Unit)
parseCExprAsUnit expr =
    case expr of
      CBinary CMulOp e1 e2 _ ->
          do u1 <- parseCExprAsUnit e1
             u2 <- parseCExprAsUnit e2
             return (liftM2 Unit.mul u1 u2)
      CBinary CDivOp e1 e2 _ ->
          do u1 <- parseCExprAsUnit e1
             u2 <- parseCExprAsUnit e2
             return (liftM2 Unit.div u1 u2)
      CVar (Ident name _ _) _ -> return (Just (Unit.fundamental name))
      CConst (CIntConst (CInteger 1 _ _) _) -> return (Just Unit.one)
      _ -> do err expr ("Can't parse expression as unit: " ++ show (pretty expr))
              return Nothing

instance FindUnit CDeclSpec where
    findUnit declSpec =
        case declSpec of
          CStorageSpec _ -> return Nothing
          CTypeSpec typeSpec -> findUnit typeSpec
          CTypeQual typeQual -> findUnit typeQual

instance FindUnit CTypeSpec where
    findUnit typeSpec =
        case typeSpec of
          CVoidType _ -> return Nothing
          CCharType _ -> return Nothing
          CShortType _ -> return Nothing
          CIntType _ -> return Nothing
          CLongType _ -> return Nothing
          CFloatType _ -> return Nothing
          CDoubleType _ -> return Nothing
          CSignedType _ -> return Nothing
          CUnsigType _ -> return Nothing
          CBoolType _ -> return Nothing
          CComplexType _ -> return Nothing
          CSUType _ _ -> return Nothing
          CEnumType _ _ -> return Nothing
          CTypeDef _ident _ ->
              do err typeSpec "typedef type specifiers not yet handled"
                 return Nothing
          CTypeOfExpr _e _ ->
              do err typeSpec "typeof(expr) type specifiers not yet handled"
                 return Nothing
          CTypeOfType _t _ ->
              do err typeSpec "typeof(type) type specifiers not yet handled"
                 return Nothing

instance FindUnit CTypeQual where
    findUnit typeQual =
        case typeQual of
          CConstQual _ -> return Nothing
          CVolatQual _ -> return Nothing
          CRestrQual _ -> return Nothing
          CInlineQual _ -> return Nothing
          CAttrQual attr -> findUnit attr

instance FindUnit a => FindUnit (Maybe a) where
    findUnit m =
        case m of
          Just a -> findUnit a
          Nothing -> return Nothing

instance FindUnit a => FindUnit [a] where
    findUnit list =
        do units <- mapMaybeM findUnit list
           case units of
             [] -> return Nothing
             [u] -> return (Just u)
             us -> do err nopos ("Conflicting units: " ++ show us)
                      return Nothing

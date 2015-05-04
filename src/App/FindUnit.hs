{-# LANGUAGE FlexibleInstances #-}

module App.FindUnit where

import App.Unit as Unit
import Control.Monad
import Control.Monad.Extra
import Language.C.Pretty
import Language.C.Data.Ident
import Language.C.Syntax.Constants
import Language.C.Syntax.AST

class FindUnit a where
    findUnit :: a -> IO (Maybe Unit)

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

parseCExprAsQ :: CExpr -> IO (Maybe Q)
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
      _ -> do print ("Can't parse expression as power: " ++ show (pretty expr))
              return Nothing

parseCExprAsUnit :: CExpr -> IO (Maybe Unit)
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
      _ -> do print ("Can't parse expression as unit: " ++ show (pretty expr))
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
          CTypeDef ident _ ->
              do putStrLn "typedef type specifiers not yet handled"
                 return Nothing
          CTypeOfExpr e _ ->
              do putStrLn "typeof(expr) type specifiers not yet handled"
                 return Nothing
          CTypeOfType t _ ->
              do putStrLn "typeof(type) type specifiers not yet handled"
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
             us -> do putStrLn ("Conflicting units: " ++ show us)
                      return Nothing

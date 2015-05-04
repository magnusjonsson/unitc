{-# LANGUAGE FlexibleInstances #-}

module App.FindType where

import App.SymTab as SymTab
import App.Type as Type
import Control.Monad
import Language.C.Pretty
import Language.C.Data.Ident
--import Language.C.Syntax.Constants
import Language.C.Syntax.AST

import App.Type
import App.SymTab

class FindType a where
    findType :: SymTab -> a -> IO (Maybe Type)

instance FindType CExpr where
    findType st expr =
        case expr of
          CComma es _ -> liftM last (mapM (findType st) es)
          CAssign op e1 e2 _ ->
              do mt1 <- findType st e1
                 mt2 <- findType st e2
                 case (mt1, mt2) of
                   (Nothing, Nothing) -> return Nothing
                   (Just t1, Just t2) ->
                       case op of
                         CAssignOp -> if t1 == t2 then return (Just t1)
                                      else do putStrLn ("Can't assign from " ++ show t2 ++ " to " ++ show t1)
                                              return Nothing
                         _ -> do putStrLn ("TODO findType CAssign " ++ show op)
                                 return Nothing
                   _ -> do putStrLn "Missing unit on one side of assignment operator"
                           return Nothing
          CCond e1 (Just e2) e3 _ -> putStrLn "TODO findType CCond" >> return Nothing
          CCond e1 Nothing e3 _ -> putStrLn "TODO findType CCond" >> return Nothing
          CBinary op e1 e2 _ ->
              do mt1 <- findType st e1
                 mt2 <- findType st e2
                 case (mt1, mt2) of
                   (Nothing, Nothing) -> return Nothing
                   (Just t1, Just t2) ->
                       case op of
                         CMulOp -> return (Type.mul t1 t2)
                         CDivOp -> return (Type.div t1 t2)
                         _ -> do putStrLn ("TODO findType CBinary " ++ show op)
                                 return Nothing
                   _ -> do putStrLn "Missing unit on one side of binary operator"
                           return Nothing
          CCast decl e _ -> putStrLn "TODO findType CCast" >> return Nothing
          CUnary op e _ -> putStrLn "TODO findType CUnary" >> return Nothing
          CSizeofExpr e _ -> putStrLn "TODO findType CSizeofExpr" >> return Nothing
          CSizeofType decl _ -> putStrLn "TODO findType CSizeofType" >> return Nothing
          CAlignofExpr e _ -> putStrLn "TODO findType CAlignofExpr" >> return Nothing
          CAlignofType decl _ -> putStrLn "TODO findType CAlignofType" >> return Nothing
          CComplexReal e _ -> putStrLn "TODO findType CComplexReal" >> return Nothing
          CComplexImag e _ -> putStrLn "TODO findType CComplexImag" >> return Nothing
          CIndex e1 e2 _ -> putStrLn "TODO findType CIndex" >> return Nothing
          CCall e1 es _ -> putStrLn "TODO findType CCall" >> return Nothing
          CMember e ident bool _ -> putStrLn "TODO findType CMember" >> return Nothing
          CVar (Ident name _ _) _ -> case SymTab.lookupVariable name st of
                                       Nothing -> do putStrLn ("Variable not in scope: " ++ name)
                                                     return Nothing
                                       Just ty -> return (Just ty)
          CConst c -> putStrLn "TODO findType CConst" >> return Nothing
          CCompoundLit decl initList _ -> putStrLn "TODO findType CCompoundLit" >> return Nothing
          CStatExpr stat _ -> putStrLn "TODO findType CStatExpr" >> return Nothing
          CLabAddrExpr ident _ -> putStrLn "TODO findType CLabAddrExpr" >> return Nothing
          CBuiltinExpr builtin -> putStrLn "TODO findType CBuiltinExpr" >> return Nothing
          

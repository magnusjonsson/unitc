{-# LANGUAGE FlexibleInstances #-}

module App.FindType where

import App.FindUnit
import App.SymTab as SymTab
import App.Type as Type
import App.Unit
import Control.Monad
import Language.C.Pretty
import Language.C.Data.Ident
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

instance FindType CStat where
    findType st stat =
        case stat of
          CLabel _ _ _ _ -> putStrLn "TODO findType CLabel" >> return Nothing
          CCase e b _ -> putStrLn "TODO findType CCase" >> return Nothing
          CCases e1 e2 b _ -> putStrLn "TODO findType CCases" >> return Nothing
          CDefault b _ -> putStrLn "TODO findType CDefault" >> return Nothing
          CExpr Nothing _ -> putStrLn "TODO findType CExpr" >> return Nothing
          CExpr (Just e) _ -> findType st e
          CCompound _ blockItems _ -> blockType st Nothing blockItems
          CIf e1 s1 Nothing _ -> putStrLn "TODO findType CIf" >> return Nothing
          CIf e1 s1 (Just s2) _ -> putStrLn "TODO findType CIf" >> return Nothing
          CSwitch e b _ -> putStrLn "TODO findType CSwitch" >> return Nothing
          CWhile e b _ _ -> putStrLn "TODO findType CWhile" >> return Nothing
          CFor init cond incr b _ -> putStrLn "TODO findType CFor" >> return Nothing
          CGoto _ _ -> putStrLn "TODO findType CGoto" >> return Nothing
          CGotoPtr e _ -> putStrLn "TODO findType CGotoPtr" >> return Nothing
          CCont _ -> putStrLn "TODO findType CCont" >> return Nothing
          CBreak _ -> putStrLn "TODO findType CBreak" >> return Nothing
          CReturn Nothing _ -> putStrLn "TODO findType CReturn" >> return Nothing
          CReturn (Just e) _ -> putStrLn "TODO findType CReturn" >> return Nothing



blockType :: SymTab -> Maybe Type -> [CBlockItem] -> IO (Maybe Type)
blockType st lastType [] = return lastType
blockType st lastType (s : r) =
    do (t, st') <- blockItemTypeAndModifiedSymTab st s
       blockType st' t r

blockItemTypeAndModifiedSymTab :: SymTab -> CBlockItem -> IO (Maybe Type, SymTab)
blockItemTypeAndModifiedSymTab st item =
    case item of
      CBlockStmt stmt -> do t <- findType st stmt
                            return (t, st)
      CBlockDecl decl -> do st' <- applyDecl st decl
                            return (Nothing, st')
      CNestedFunDef f -> do putStrLn "TODO findType CNestedFunDef"
                            return (Nothing, st)

applyDecl :: SymTab -> CDecl -> IO SymTab
applyDecl st decl =
    case decl of
      CDecl declSpecs triplets _ ->
          do unit <- findUnit declSpecs
             applyTriplets st unit triplets

type Triplet = (Maybe CDeclr, Maybe CInit, Maybe CExpr)
                           
applyTriplets :: SymTab -> Maybe Unit -> [Triplet] -> IO SymTab
applyTriplets st specUnit [] = return st
applyTriplets st specUnit (triplet : r) = do st' <- applyTriplet st specUnit triplet
                                             applyTriplets st' specUnit r


applyTriplet :: SymTab -> Maybe Unit -> Triplet -> IO SymTab
applyTriplet st specUnit (declr, initr, bitFieldSize) =
    do declrUnit <- findUnit declr
       unit <- findUnit [specUnit, declrUnit]

       -- Todo don't guess type like this... look at actual specifiers to determine if it's numeric
       let ty = case unit of
                  Nothing -> Other
                  Just u -> Numeric (Just u)
       case initr of
         Just (CInitExpr e _) ->
             do initType <- findType st e
                if initType /= Just ty then
                    putStrLn ("Can't assign from " ++ show initType ++ " to " ++ show ty)
                else
                    return ()
         Nothing -> return ()
         _ -> putStrLn ("TODO applyTriplet initr=" ++ show initr)

       case declr of
         Just (CDeclr (Just (Ident name _ _)) _ _ _ _) ->
             return (SymTab.bindVariable name ty st)
         _ -> do putStrLn ("Unhandled CDeclr: " ++ show declr)
                 return st

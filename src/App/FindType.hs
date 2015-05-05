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
          CReturn e _ ->
              do ty <- case e of
                         Nothing -> return (Just Other)
                         Just e' -> findType st e'
                 case returnType st of
                   Nothing -> putStrLn "Encountered return statement but not sure what return type is expected!"
                   Just r ->
                       if ty /= Just r then
                           putStrLn ("Type " ++ show ty ++ " does not agree with return type " ++ show r)
                       else
                           return ()
                 return Nothing

instance FindType CDeclSpec where
    findType st declSpec =
        case declSpec of
          CStorageSpec _ -> return Nothing
          CTypeSpec typeSpec -> findType st typeSpec
          CTypeQual typeQual -> findType st typeQual

instance FindType CTypeSpec where
    findType st typeSpec =
        case typeSpec of
          CVoidType _ -> return (Just Other)
          CCharType _ -> return (Just (Numeric Nothing))
          CShortType _ -> return (Just (Numeric Nothing))
          CIntType _ -> return (Just (Numeric Nothing))
          CLongType _ -> return (Just (Numeric Nothing))
          CFloatType _ -> return (Just (Numeric Nothing))
          CDoubleType _ -> return (Just (Numeric Nothing))
          CSignedType _ -> return (Just (Numeric Nothing))
          CUnsigType _ -> return (Just (Numeric Nothing))
          CBoolType _ -> return (Just Other)
          CComplexType _ -> return (Just (Numeric Nothing))
          CTypeDef ident _ ->
              do putStrLn "CTypeSpec: typedef type specifiers not yet handled"
                 return Nothing
          CTypeOfExpr e _ ->
              do putStrLn "CTypeSpec: typeof(expr) type specifiers not yet handled"
                 return Nothing
          CTypeOfType t _ ->
              do putStrLn "CTypeSpec: typeof(type) type specifiers not yet handled"
                 return Nothing

instance FindType CTypeQual where
    findType st typeQual =
        case typeQual of
          CConstQual _ -> return Nothing
          CVolatQual _ -> return Nothing
          CRestrQual _ -> return Nothing
          CInlineQual _ -> return Nothing
          CAttrQual attr -> findType st attr

instance FindType CAttr where
    findType st attr =
        do unit <- findUnit attr
           case unit of
             Nothing -> return Nothing
             Just u -> return (Just (Numeric (Just u)))

instance FindType a => FindType [a] where
    findType st list =
        do ts <- mapM (findType st) list
           return (foldl Type.mergeMaybe Nothing ts)

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
          do ty <- findType st declSpecs
             applyTriplets st ty triplets

type Triplet = (Maybe CDeclr, Maybe CInit, Maybe CExpr)
                           
applyTriplets :: SymTab -> Maybe Type -> [Triplet] -> IO SymTab
applyTriplets st specType triplets =
    case triplets of
      [] -> return st
      (triplet : r) -> do st' <- applyTriplet st specType triplet
                          applyTriplets st' specType r

applyTriplet :: SymTab -> Maybe Type -> Triplet -> IO SymTab
applyTriplet st specType (declr, initr, bitFieldSize) =
    do ty <- case declr of
               Just (CDeclr _ derivedDeclarators _ attrs _) ->
                   do attrType <- findType st attrs
                      deriveType derivedDeclarators (Type.mergeMaybe specType attrType)
               Nothing -> return specType
       case initr of
         Just (CInitExpr e _) ->
             do initType <- findType st e
                if initType /= ty then
                    putStrLn ("Can't assign from " ++ show initType ++ " to " ++ show ty)
                else
                    return ()
         Nothing -> return ()
         _ -> putStrLn ("TODO applyTriplet initr=" ++ show initr)
       case declr of
         Just (CDeclr (Just (Ident name _ _)) _ _ _ _) ->
             case ty of
               Just ty' -> return (SymTab.bindVariable name ty' st)
               Nothing -> do putStrLn ("Could not infer type for " ++ name)
                             return st
         _ -> do putStrLn ("Unhandled CDeclr: " ++ show declr)
                 return st

deriveType :: [CDerivedDeclr] -> Maybe Type -> IO (Maybe Type)
deriveType ds ty =
    case ds of
      [] -> return ty
      (d : dr) -> do ty' <- deriveType dr ty
                     deriveType1 d ty'

deriveType1 :: CDerivedDeclr -> Maybe Type -> IO (Maybe Type)
deriveType1 d ty =
    case d of
      CPtrDeclr _ _ -> return ty
      CArrDeclr _ _ _ -> return ty
      CFunDeclr _ _ _ ->
          do putStrLn "TODO deriveType CFunDeclr"
             return ty

applyFunDef :: SymTab -> CFunDef -> IO SymTab
applyFunDef st f =
    case f of
      CFunDef specs (CDeclr ident derivedDeclrs _ attrs _) argDecls body _ ->
          do specType <- findType st specs
             attrType <- findType st attrs
             returnType <- deriveType derivedDeclrs (Type.mergeMaybe specType attrType)
             st' <- case (ident, returnType) of
                      (Just (Ident name _ _), Just rt) -> return (SymTab.bindVariable name rt st)
                      (Nothing, _) -> do putStrLn "Strange fundef! Function has no name!"
                                         return st
                      (_, Nothing) -> do putStrLn "Could not determine return type"
                                         return st
             putStrLn "TODO add args to symtab"
             _ <- findType (SymTab.setReturnType returnType st') body
             return st'

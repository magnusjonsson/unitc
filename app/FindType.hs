{-# LANGUAGE FlexibleInstances #-}

module FindType where

import FindUnit
import Monad.Analysis
import qualified SymTab
import Type
import qualified Unit

import Control.Monad
import Language.C.Pretty
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Syntax.AST

class FindType a where
    findType :: a -> Analysis (Maybe Type)

instance FindType CExpr where
    findType expr =
        case expr of
          CComma es _ -> liftM last (mapM findType es)
          CAssign op e1 e2 _ ->
              do mt1 <- findType e1
                 mt2 <- findType e2
                 case (mt1, mt2) of
                   (Just t1, Just t2) ->
                       case op of
                         CAssignOp -> if Type.assignable t1 t2 then return (Just t1)
                                      else do err expr ("Can't assign from " ++ show t2 ++ " to " ++ show t1)
                                              return Nothing
                         _ -> do err expr ("TODO findType CAssign " ++ show op)
                                 return Nothing
                   _ -> return Nothing
          CCond e1 (Just e2) e3 _ ->
              do t1 <- findType e1
                 _ <- combineTypes expr "don't match" Type.add t1 (Just Type.one)
                 t2 <- findType e2
                 t3 <- findType e3
                 combineTypes expr "must have same unit" Type.merge t1 t2
          CCond e1 Nothing e3 _ -> err expr "TODO findType CCond" >> return Nothing
          CBinary op e1 e2 _ ->
              do t1 <- findType e1
                 t2 <- findType e2
                 case op of
                   CEqOp -> do _ <- combineTypes expr "can't be compared" Type.merge t1 t2; return (Just Type.one)
                   CNeqOp -> do _ <- combineTypes expr "can't be compared" Type.merge t1 t2; return (Just Type.one)
                   CLeOp -> do _ <- combineTypes expr "can't be compared" Type.add t1 t2; return (Just Type.one)
                   CGrOp -> do _ <- combineTypes expr "can't be compared" Type.add t1 t2; return (Just Type.one)
                   CLeqOp -> do _ <- combineTypes expr "can't be compared" Type.add t1 t2; return (Just Type.one)
                   CGeqOp -> do _ <- combineTypes expr "can't be compared" Type.add t1 t2; return (Just Type.one)
                   CAddOp -> combineTypes expr "can't be added" Type.add t1 t2
                   CSubOp -> combineTypes expr "can't be subtracted" Type.sub t1 t2
                   CMulOp -> combineTypes expr "can't be multiplied" Type.mul t1 t2
                   CDivOp -> combineTypes expr "can't be divided" Type.div t1 t2
                   CShlOp -> do _ <- combineTypes expr "can't be unified" Type.add t2 (Just Type.one); return t1
                   CShrOp -> do _ <- combineTypes expr "can't be unified" Type.add t2 (Just Type.one); return t1
                   COrOp -> combineTypes expr "can't be unified" Type.add t1 t2
                   CAndOp -> combineTypes expr "can't be unified" Type.add t1 t2
                   CLorOp -> combineTypes expr "can't be unified" Type.add t1 t2
                   CLndOp -> combineTypes expr "can't be unified" Type.add t1 t2
                   _ -> do err expr ("TODO findType CBinary " ++ show op)
                           return Nothing
          CCast (CDecl specs triplets _) e _ ->
            do td <- findType specs
               te <- findType e
               td' <- case triplets of
                        [] -> return td
                        [(Just declr, Nothing, Nothing)] -> deriveTypeFromCDeclr td declr
                        _ -> err expr "TODO findType CCast with strange triplets" >> return Nothing
               return (fmap Type.monomorphize td')
          CUnary op e _ ->
              do t <- findType e
                 case op of
                   CPreIncOp -> combineTypes expr "can't be added" Type.add t (Just Type.one)
                   CPreDecOp -> combineTypes expr "can't be subtracted" Type.add t (Just Type.one)
                   CPostIncOp -> combineTypes expr "can't be added" Type.add t (Just Type.one)
                   CPostDecOp -> combineTypes expr "can't be subtracted" Type.add t (Just Type.one)
                   CAdrOp -> return t
                   CIndOp -> return t
                   CPlusOp -> combineTypes expr "can't be multiplied" Type.mul t (Just Type.one)
                   CMinOp -> combineTypes expr "can't be multiplied" Type.mul t (Just Type.one)
                   CCompOp -> combineTypes expr "can't be added" Type.mul t (Just Type.one) >> return (Just Type.one)
                   CNegOp -> combineTypes expr "can't be added" Type.mul t (Just Type.one) >> return (Just Type.one)
          CSizeofExpr e _ -> err expr "TODO findType CSizeofExpr" >> return Nothing
          CSizeofType decl _ -> err expr "TODO findType CSizeofType" >> return Nothing
          CAlignofExpr e _ -> err expr "TODO findType CAlignofExpr" >> return Nothing
          CAlignofType decl _ -> err expr "TODO findType CAlignofType" >> return Nothing
          CComplexReal e _ -> err expr "TODO findType CComplexReal" >> return Nothing
          CComplexImag e _ -> err expr "TODO findType CComplexImag" >> return Nothing
          CIndex e1 e2 _ -> do t1 <- findType e1
                               t2 <- findType e1
                               return t1
          CCall e1 es _ ->
              do t1 <- findType e1
                 actuals <- mapM findType es
                 case t1 of
                   Nothing -> return Nothing
                   Just (Fun rt formals acceptsVarArgs) ->
                       do checkArgs (nodeInfo expr) actuals formals acceptsVarArgs
                          return (Just rt)
                   Just _ ->
                       do err expr ("Non-function called as a function: " ++ show (pretty e1))
                          return Nothing
          CMember e (Ident field _ _) _ _ -> do ty <- findType e
                                                st <- getSymTab
                                                case ty of
                                                  Nothing -> return Nothing
                                                  Just (Struct tag) ->
                                                      case SymTab.lookupTag tag st of
                                                        Nothing -> do err expr ("Could not find struct tag " ++ tag ++ " in symbol table")
                                                                      return Nothing
                                                        Just fields ->
                                                            case SymTab.lookupField field fields of
                                                              Nothing -> do err expr ("No such field " ++ field ++ " in tag " ++ tag)
                                                                            return Nothing
                                                              Just ty -> return (Just ty)
                                                  _ -> do err expr "Can't get field of non-struct"
                                                          return Nothing
          CVar (Ident name _ _) _ -> do st <- getSymTab
                                        case SymTab.lookupVariable name st of
                                          Nothing -> do err expr ("Variable not in scope: " ++ name)
                                                        return Nothing
                                          Just ty -> return (Just ty)
          CConst c -> findType c
          CCompoundLit decl initList _ -> err expr "TODO findType CCompoundLit" >> return Nothing
          CStatExpr stat _ -> err expr "TODO findType CStatExpr" >> return Nothing
          CLabAddrExpr ident _ -> err expr "TODO findType CLabAddrExpr" >> return Nothing
          CBuiltinExpr builtin -> err expr "TODO findType CBuiltinExpr" >> return Nothing

combineTypes :: Pos a => a -> String -> (Type -> Type -> Maybe Type) -> Maybe Type -> Maybe Type -> Analysis (Maybe Type)
combineTypes pos msg f t1 t2 =
    case (t1, t2) of
      (Just t1', Just t2') ->
          case f t1' t2' of
            Nothing -> err pos ("type " ++ show t1 ++ " and " ++ show t2 ++ " " ++ msg) >> return Nothing
            Just ty -> return (Just ty)
      _ -> return Nothing

checkCompatibility :: Pos a => a -> Type -> Type -> Analysis ()
checkCompatibility pos t1 t2 =
    if t1 /= t2 then
        err pos ("incompatible types: " ++ show t1 ++ ", " ++ show t2)
    else
        return ()

checkArgs :: NodeInfo -> [Maybe Type] -> [(Maybe String, Type)] -> Bool -> Analysis ()
checkArgs node actuals formals acceptVarArgs =
    case (actuals, formals, acceptVarArgs) of
      ([], [], _) -> return ()
      ([], _, _) -> err node "Too few args"
      (_, [], True) -> return ()
      (_, [], False) -> err node "Too many args"
      (Nothing : as, f : fs, _) ->
          checkArgs node as fs acceptVarArgs
      (Just a : as, (fname, ftype) : fs, _) ->
          do if not (Type.assignable ftype a) then
                 err node ("Argument type mismatch. Found " ++ show a ++ ", expected " ++ show ftype ++ ".")
             else
                 return ()
             checkArgs node as fs acceptVarArgs

instance FindType CConst where
    findType c =
        case c of
          CIntConst _ _ -> return (Just (Numeric (Just Unit.one)))
          CCharConst _ _ -> return (Just (Numeric (Just Unit.one)))
          CFloatConst _ _ -> return (Just (Numeric (Just Unit.one)))
          CStrConst _ _ -> return (Just (Numeric (Just Unit.one)))

instance FindType CStat where
    findType stat =
        case stat of
          CLabel _ _ _ _ -> err stat "TODO findType CLabel" >> return Nothing
          CCase e b _ -> err stat "TODO findType CCase" >> return Nothing
          CCases e1 e2 b _ -> err stat "TODO findType CCases" >> return Nothing
          CDefault b _ -> err stat "TODO findType CDefault" >> return Nothing
          CExpr Nothing _ -> err stat "TODO findType CExpr" >> return Nothing
          CExpr (Just e) _ -> findType e
          CCompound _ blockItems _ -> blockType blockItems
          CIf e s1 Nothing _ -> do te <- findType e
                                   t1 <- findType s1
                                   return Nothing
          CIf e s1 (Just s2) _ -> do te <- findType e
                                     t1 <- findType s1
                                     t1 <- findType s2
                                     return Nothing
          CSwitch e b _ -> err stat "TODO findType CSwitch" >> return Nothing
          CWhile e b _ _ -> do te <- findType e
                               tb <- findType b
                               return Nothing
          CFor init cond incr body _ ->
            do st <- getSymTab
               case init of
                 Left Nothing -> return ()
                 Left (Just expr) -> findType expr >> return ()
                 Right decl -> applyCDecl decl
               case cond of
                 Nothing -> return ()
                 Just cond' -> findType cond' >> return ()
               case incr of
                 Nothing -> return ()
                 Just incr' -> findType incr' >> return ()
               ty <- findType body
               setSymTab st
               return ty
          CGoto _ _ -> err stat "TODO findType CGoto" >> return Nothing
          CGotoPtr e _ -> err stat "TODO findType CGotoPtr" >> return Nothing
          CCont _ -> return Nothing
          CBreak _ -> return Nothing
          CReturn e _ ->
              do ty <- case e of
                         Nothing -> return (Just Other)
                         Just e' -> findType e'
                 st <- getSymTab
                 case SymTab.returnType st of
                   Nothing -> err stat "Encountered return statement but not sure what return type is expected!"
                   Just r ->
                       if ty /= Just r then
                           err stat ("Type " ++ show ty ++ " does not agree with return type " ++ show r)
                       else
                           return ()
                 return Nothing
          CAsm _ _ -> err stat "TODO findType CAsm" >> return Nothing

instance FindType CDeclSpec where
    findType declSpec =
        case declSpec of
          CStorageSpec _ -> return Nothing
          CTypeSpec typeSpec -> findType typeSpec
          CTypeQual typeQual -> findType typeQual

instance FindType CTypeSpec where
    findType typeSpec =
        case typeSpec of
          CVoidType _ -> return (Just Void)
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
          CSUType csu _ -> findType csu
          CEnumType ce _ -> findType ce
          CTypeDef (Ident name _ _) _ ->
              do st <- getSymTab
                 case SymTab.lookupType name st of
                   Just ty -> return (Just ty)
                   Nothing -> do err typeSpec ("Could not find typedef: " ++ name)
                                 return Nothing
          CTypeOfExpr e _ ->
              do err typeSpec "CTypeSpec: typeof(expr) type specifiers not yet handled"
                 return Nothing
          CTypeOfType t _ ->
              do err typeSpec "CTypeSpec: typeof(type) type specifiers not yet handled"
                 return Nothing
--          _ -> do err typeSpec ("Missing case: " ++ show typeSpec)
--                  return Nothing

instance FindType CStructUnion where
    findType csu =
        do name <-
               case csu of
                 CStruct _ (Just (Ident name _ _)) _ _ _ ->
                     return name
                 CStruct _ Nothing _ _ _ ->
                     gensym
           case csu of
             CStruct _ _ Nothing _ _ -> return ()
             CStruct _ _ (Just fields) _ _ ->
                 do parent <- getSymTab
                    setSymTab (SymTab.newScope parent)
                    mapM_ applyCDecl fields
                    fieldSymTab <- getSymTab
                    setSymTab (SymTab.bindTag name (SymTab.variables fieldSymTab) parent)
           return (Just (Struct name))

instance FindType CEnum where
    findType enum =
        -- we ignore the name of enums. All we care about is that they're numeric.
        -- we don't support unit annotations on enums.
        case enum of
          CEnum _ Nothing _ _ -> return (Just (Numeric Nothing))
          CEnum _ (Just bindings) _ _ ->
              do mapM_ applyEnumBinding bindings
                 return (Just (Numeric (Just Unit.one)))

applyEnumBinding :: (Ident, Maybe CExpr) -> Analysis ()
applyEnumBinding (ident, maybeExpr) =
    case ident of
      Ident name _ _ ->
          do case maybeExpr of
               Nothing -> return ()
               Just e -> do ty <- findType e
                            case ty of
                              Nothing -> return ()
                              Just ty' ->
                                  if ty' /= Numeric (Just Unit.one) then
                                      err e ("Expected numeric with unit 1, got " ++ show ty)
                                  else
                                      return ()
             modifySymTab (SymTab.bindVariable name (Numeric (Just Unit.one)))

instance FindType CTypeQual where
    findType typeQual =
        case typeQual of
          CConstQual _ -> return Nothing
          CVolatQual _ -> return Nothing
          CRestrQual _ -> return Nothing
          CInlineQual _ -> return Nothing
          CAttrQual attr -> findType attr

instance FindType CAttr where
    findType attr =
        do unit <- findUnit attr
           case unit of
             Nothing -> return Nothing
             Just u -> return (Just (Numeric (Just u)))

instance FindType a => FindType [a] where
    findType list =
        do ts <- mapM findType list
           return (foldl Type.mergeMaybe Nothing ts)

blockType :: [CBlockItem] -> Analysis (Maybe Type)
blockType items =
    do st <- getSymTab
       types <- mapM applyBlockItem items
       setSymTab st
       case types of
         [] -> return Nothing
         _ -> return (last types)

applyBlockItem :: CBlockItem -> Analysis (Maybe Type)
applyBlockItem item =
    case item of
      CBlockStmt stmt -> findType stmt
      CBlockDecl decl -> do applyCDecl decl
                            return Nothing
      CNestedFunDef f -> do err f "TODO findType CNestedFunDef"
                            return Nothing

applyCDecl :: CDecl -> Analysis ()
applyCDecl decl =
    case decl of
      CDecl declSpecs triplets _ ->
          do ty <- findType declSpecs
             mapM_ (applyTriplet decl ty (isTypeDef declSpecs)) triplets

type Triplet = (Maybe CDeclr, Maybe CInit, Maybe CExpr)
                           
isTypeDef :: [CDeclSpec] -> Bool
isTypeDef =
  any (\ spec ->
        case spec of
         (CStorageSpec (CTypedef _)) -> True
         _ -> False)

deriveTypeFromCDeclr :: Maybe Type -> CDeclr -> Analysis (Maybe Type)
deriveTypeFromCDeclr declSpecTy (CDeclr _ derivedDeclrs _ attrs _) =
    do attrType <- findType attrs
       deriveType derivedDeclrs (Type.mergeMaybe declSpecTy attrType)

applyTriplet :: Pos a => a -> Maybe Type -> Bool -> Triplet -> Analysis ()
applyTriplet node declSpecTy isTypeDef (declr, initr, bitFieldSize) =
    do ty <- case declr of
               Just declr' -> deriveTypeFromCDeclr declSpecTy declr'
               Nothing -> return declSpecTy
       case initr of
         Just (CInitExpr e _) ->
             do initType <- findType e
                case (ty, initType) of
                  (Just ty', Just initType') ->
                      if Type.assignable ty' initType' then
                        return ()
                      else
                        err e ("Can't assign from " ++ show initType' ++ " to " ++ show ty')
                  _ -> return ()
         Nothing -> return ()
         _ -> err node ("TODO applyTriplet initr=" ++ show initr)
       case declr of
         Just declr' ->
           case declr' of
             CDeclr (Just (Ident name _ _)) _ _ _ _ ->
               case ty of
                Just ty' ->
                  if isTypeDef then
                      modifySymTab (SymTab.bindType name ty')
                  else
                      modifySymTab (SymTab.bindVariable name ty')
                Nothing -> err declr' ("Could not infer type for " ++ name)
             _ -> err node ("Unhandled CDeclr: " ++ show declr)
         Nothing -> return () -- happens in /usr/include/bits/waitstatus.h

deriveType :: [CDerivedDeclr] -> Maybe Type -> Analysis (Maybe Type)
deriveType ds ty =
    case ds of
      [] -> return (fmap Type.monomorphize ty)
      (d : dr) -> do ty' <- deriveType dr ty
                     deriveType1 d ty'
  
deriveType1 :: CDerivedDeclr -> Maybe Type -> Analysis (Maybe Type)
deriveType1 d maybeTy =
    case maybeTy of
      Nothing -> return Nothing
      Just ty ->
          case d of
            CPtrDeclr _ _ -> return (Just ty)
            CArrDeclr _ _ _ -> return (Just ty)
            CFunDeclr (Left _) _ _ ->
              do err d "TODO old-style function declaration"
                 return Nothing
            CFunDeclr (Right (decls, varArgs)) attrs _ ->
              case decls of
                [CDecl [CTypeSpec (CVoidType _)] [] _] ->
                  return (Just (Fun ty [] varArgs))
                _ ->
                  do maybeArgs <- mapM argNameAndType decls
                     case sequence maybeArgs of -- maybe monad
                       Nothing -> return Nothing
                       Just args -> return (Just (Fun ty args varArgs))

argNameAndType :: CDecl -> Analysis (Maybe (Maybe String, Type))
argNameAndType cdecl =
    case cdecl of
      CDecl specs [] _ -> do ty <- findType specs
                             case ty of
                               Just ty' -> return (Just (Nothing, ty'))
                               Nothing -> return Nothing
      CDecl specs [(Just (CDeclr maybeIdent derivedDeclrs _ attrs _), Nothing, Nothing)] _ ->
          do specType <- findType specs
             attrType <- findType attrs
             ty <- deriveType derivedDeclrs (Type.mergeMaybe specType attrType)
             case ty of
               Nothing -> return Nothing
               Just ty' -> 
                 do let maybeName = fmap (\(Ident name _ _) -> name) maybeIdent
                    return (Just (maybeName, ty'))
      _ -> do err cdecl "TODO strange arg declaration"
              return Nothing

applyCFunDef :: CFunDef -> Analysis ()
applyCFunDef f =
    case f of
      CFunDef specs (CDeclr ident derivedDeclrs _ attrs _) argDecls body _ ->
          do specType <- findType specs
             attrType <- findType attrs
             ty <- deriveType derivedDeclrs (Type.mergeMaybe specType attrType)
             case (ident, ty) of
               (Just (Ident name _ _), Just ty') ->
                   do modifySymTab (SymTab.bindVariable name ty')
               (Nothing, _) -> err f "Strange fundef! Function has no name!"
               (_, Nothing) -> err f "Could not determine function type"

             -- save symtab before processing args and body
             outsideScope <- getSymTab

             case ty of
               Just (Fun rt args _) ->
                   do modifySymTab (SymTab.setReturnType (Just rt))
                      forM_ args $ \ arg ->
                        case arg of
                          (Just argName, argTy) -> modifySymTab (SymTab.bindVariable argName argTy)
                          (Nothing, argTy) -> err f "Missing argument name"
               _ -> return ()

             _ <- findType body

             setSymTab outsideScope

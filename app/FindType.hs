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
import Language.C.Syntax.Constants

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
                     do mt1' <- case op of
                          CAssignOp -> return mt2
                          CAddAssOp -> return (Type.add t1 t2)
                          CSubAssOp -> return (Type.sub t1 t2)
                          CMulAssOp -> return (Type.mul t1 t2)
                          CDivAssOp -> return (Type.div t1 t2)
                          CRmdAssOp -> return (Type.rem t1 t2)
                          CShlAssOp -> return (Type.shl t1 t2)
                          CShrAssOp -> return (Type.shr t1 t2)
                          COrAssOp -> return (Type.or t1 t2)
                          CAndAssOp -> return (Type.and t1 t2)
                          CXorAssOp -> return (Type.xor t1 t2)
                        case mt1' of
                          Nothing -> return ()
                          Just t1' ->
                            if Type.assignable t1 t1' then
                              return ()
                            else
                              err expr ("Can't assign to type " ++ show t1 ++ " from type " ++ show t1')
                   _ -> return ()
                 return mt1
          CCond e1 (Just e2) e3 _ ->
              do t1 <- findType e1
                 _ <- combineTypes expr "don't match" Type.add t1 (Just Type.one)
                 t2 <- findType e2
                 t3 <- findType e3
                 combineTypes expr "must have same unit" Type.merge t2 t3
          CCond e1 Nothing e3 _ -> err expr "TODO findType CCond" >> return Nothing
          CBinary op e1 e2 _ ->
              do t1 <- findType e1
                 t2 <- findType e2
                 case op of
                   CEqOp -> do _ <- combineTypes expr "can't be compared" Type.cmp t1 t2; return (Just Type.one)
                   CNeqOp -> do _ <- combineTypes expr "can't be compared" Type.cmp t1 t2; return (Just Type.one)
                   CLeOp -> do _ <- combineTypes expr "can't be compared" Type.cmp t1 t2; return (Just Type.one)
                   CGrOp -> do _ <- combineTypes expr "can't be compared" Type.cmp t1 t2; return (Just Type.one)
                   CLeqOp -> do _ <- combineTypes expr "can't be compared" Type.cmp t1 t2; return (Just Type.one)
                   CGeqOp -> do _ <- combineTypes expr "can't be compared" Type.cmp t1 t2; return (Just Type.one)
                   CAddOp -> combineTypes expr "can't be added" Type.add t1 t2
                   CSubOp -> combineTypes expr "can't be subtracted" Type.sub t1 t2
                   CMulOp -> combineTypes expr "can't be multiplied" Type.mul t1 t2
                   CDivOp -> combineTypes expr "can't be divided" Type.div t1 t2
                   CRmdOp -> combineTypes expr "can't be used in remainder operation" Type.rem t1 t2
                   CShlOp -> do _ <- combineTypes expr "can't be unified" Type.shl t2 (Just Type.one); return t1
                   CShrOp -> do _ <- combineTypes expr "can't be unified" Type.shr t2 (Just Type.one); return t1
                   COrOp -> combineTypes expr "can't be unified" Type.or t1 t2
                   CAndOp -> combineTypes expr "can't be unified" Type.and t1 t2
                   CLorOp -> combineTypes expr "can't be unified" Type.lor t1 t2
                   CLndOp -> combineTypes expr "can't be unified" Type.land t1 t2
                   CXorOp -> combineTypes expr "can't be unified" Type.xor t1 t2
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
                   CAdrOp -> case t of
                               Nothing -> return Nothing
                               Just t' -> return (Just (Ptr t'))
                   CIndOp -> case t of
                               Nothing -> return Nothing
                               Just (Ptr t') -> return (Just t')
                               Just _ -> err expr ("Can't dereference non-pointer type: " ++ show t) >> return Nothing
                   CPlusOp -> combineTypes expr "can't be multiplied" Type.mul t (Just Type.one)
                   CMinOp -> combineTypes expr "can't be multiplied" Type.mul t (Just Type.one)
                   CCompOp -> combineTypes expr "can't be added" Type.mul t (Just Type.one) >> return (Just Type.one)
                   CNegOp -> combineTypes expr "can't be added" Type.mul t (Just Type.one) >> return (Just Type.one)
          CSizeofExpr e _ -> do _ <- findType e
                                return (Just one)
          CSizeofType decl _ -> return (Just one)
          CAlignofExpr e _ -> err expr "TODO findType CAlignofExpr" >> return Nothing
          CAlignofType decl _ -> err expr "TODO findType CAlignofType" >> return Nothing
          CComplexReal e _ -> err expr "TODO findType CComplexReal" >> return Nothing
          CComplexImag e _ -> err expr "TODO findType CComplexImag" >> return Nothing
          CIndex e1 e2 _ -> do t1 <- findType e1
                               t2 <- findType e2
                               case t2 of
                                 Nothing -> return ()
                                 Just t2' ->
                                   if Type.assignable Type.one t2' then
                                     return ()
                                   else
                                     err expr ("Subscript type is not numeric with unit 1: " ++ show t2)
                               case t1 of
                                 Nothing -> return Nothing
                                 Just (Arr t1') -> return (Just t1')
                                 Just (Ptr t1') -> return (Just t1')
                                 Just _ -> err expr ("Not an array or pointer: " ++ show t1) >> return Nothing
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
          CMember e (Ident field _ _) deref _ ->
            do ty <- findType e
               st <- getSymTab
               ty' <- if deref then
                        case ty of
                          Nothing -> return Nothing
                          Just (Ptr t) -> return (Just t)
                          Just t -> err expr ("Can't dereference non-reference type " ++ show t) >> return Nothing
                      else
                        return ty
               case ty' of
                 Nothing -> return Nothing
                 Just (Struct tag) ->
                   case SymTab.lookupTag tag st of
                     Nothing -> do err expr ("Could not find struct/union tag " ++ tag ++ " in symbol table")
                                   return Nothing
                     Just fields ->
                       case SymTab.lookupField field fields of
                         Nothing -> do err expr ("No such field " ++ field ++ " in tag " ++ tag)
                                       return Nothing
                         Just ty'' -> return (Just ty'')
                 _ -> do err expr ("Can't get field " ++ field ++ " of non-struct type " ++ show ty')
                         return Nothing
          CVar (Ident name _ _) _ -> do st <- getSymTab
                                        case SymTab.lookupVariable name st of
                                          Nothing -> do err expr ("Variable not in scope: " ++ name)
                                                        return Nothing
                                          Just ty -> return (Just ty)
          CConst c -> findType c
          CCompoundLit (CDecl specs triplets _) initList _ ->
            do tspecs <- findType specs
               case triplets of
                 [] -> return ()
                 _ -> err expr "TODO CCompoundLit with triplets"
               checkInitList tspecs initList
               return tspecs
          CStatExpr stat _ -> findType stat
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
          CIntConst (CInteger i _ f) _ ->
            if i == 0 && f == noFlags then
              return (Just Zero)
            else
              return (Just (Numeric (Just Unit.one)))
          CCharConst _ _ -> return (Just (Numeric (Just Unit.one)))
          CFloatConst _ _ -> return (Just (Numeric (Just Unit.one)))
          CStrConst _ _ -> return (Just (Ptr (Numeric (Just Unit.one))))

instance FindType CStat where
    findType stat =
        case stat of
          CLabel _ _ _ _ -> err stat "TODO findType CLabel" >> return (Just Void)
          CCase e b _ -> return (Just Void)
          CCases e1 e2 b _ -> return (Just Void)
          CDefault b _ -> return (Just Void)
          CExpr Nothing _ -> err stat "TODO findType CExpr" >> return (Just Void)
          CExpr (Just e) _ -> findType e
          CCompound _ blockItems _ -> blockType blockItems
          CIf e s1 Nothing _ -> do te <- findType e
                                   t1 <- findType s1
                                   return (Just Void)
          CIf e s1 (Just s2) _ -> do te <- findType e
                                     t1 <- findType s1
                                     t2 <- findType s2
                                     return (Just Void)
          CSwitch e b _ -> do te <- findType e
                              case te of
                                Nothing -> return ()
                                Just te' ->
                                  if Type.assignable (Type.one) te' then
                                    return ()
                                  else
                                    err stat "Switch expression must be numeric of unit 1"
                              tb <- findType b
                              return (Just Void)
          CWhile e b _ _ -> do te <- findType e
                               tb <- findType b
                               return (Just Void)
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
               return (Just Void)
          CGoto _ _ -> err stat "TODO findType CGoto" >> return (Just Void)
          CGotoPtr e _ -> err stat "TODO findType CGotoPtr" >> return (Just Void)
          CCont _ -> return (Just Void)
          CBreak _ -> return (Just Void)
          CReturn e _ ->
              do ty <- case e of
                         Nothing -> return (Just Void)
                         Just e' -> findType e'
                 st <- getSymTab
                 case SymTab.returnType st of
                   Nothing -> err stat "Encountered return statement but not sure what return type is expected!"
                   Just r ->
                     case ty of
                       Nothing -> return ()
                       Just ty' ->
                         if Type.assignable r ty' then
                           return ()
                         else
                           err stat ("Type " ++ show ty ++ " does not agree with return type " ++ show r)
                 return (Just Void)
          CAsm _ _ -> return (Just Void)

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
          CBoolType _ -> return (Just (Numeric Nothing))
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
                                  if Type.assignable Type.one ty' then
                                    return ()
                                  else
                                    err e ("Expected numeric with unit 1, got " ++ show ty')
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

checkInitializer :: Maybe Type -> CInit -> Analysis ()
checkInitializer ty initr =
  case initr of
    CInitExpr e _ ->
      do initType <- findType e
         case (ty, initType) of
           (Just ty', Just initType') ->
             if Type.assignable ty' initType' then
               return ()
             else
               err e ("Can't assign to " ++ show ty' ++ " from " ++ show initType')
           _ -> return ()
    CInitList initList _ -> checkInitList ty initList

checkInitList :: Maybe Type -> CInitList -> Analysis ()
checkInitList ty initList =
  forM_ initList $ \(designators, initr) -> checkInitListItem ty designators initr

checkInitListItem :: Maybe Type -> [CDesignator] -> CInit -> Analysis ()
checkInitListItem ty designators initr =
  case designators of
    [] -> err initr "TODO checkInitListItem initializer without designators"
    _ : _ -> do ty' <- walkType ty designators
                checkInitializer ty' initr

walkType :: Maybe Type -> [CDesignator] -> Analysis (Maybe Type)
walkType ty designators =
  foldM walkType1 ty designators

walkType1 :: Maybe Type -> CDesignator -> Analysis (Maybe Type)
walkType1 ty designator =
  case ty of
    Nothing -> return Nothing
    Just ty' ->
      case designator of
        CMemberDesig (Ident field _ _) _ ->
          case ty' of
            Struct tag ->
              do st <- getSymTab
                 case SymTab.lookupTag tag st of
                   Nothing -> err designator ("Struct/union definition not in scope: " ++ tag) >> return Nothing
                   Just fields ->
                     case SymTab.lookupField field fields of
                       Nothing -> err designator ("Struct/union " ++ tag ++ " does not have a field named " ++ field) >> return Nothing
                       Just ty'' -> return (Just ty'')
            _ -> err designator ("Type is not a struct/union: " ++ show ty') >> return Nothing
        CArrDesig e _ -> err designator "TODO walkType1 CArrDesig" >> return Nothing
        CRangeDesig e1 e2 _ -> err designator "TODO walkType1 CRangeDesig" >> return Nothing

applyTriplet :: Pos a => a -> Maybe Type -> Bool -> Triplet -> Analysis ()
applyTriplet node declSpecTy isTypeDef (declr, initr, bitFieldSize) =
    do ty <- case declr of
               Just declr' -> deriveTypeFromCDeclr declSpecTy declr'
               Nothing -> return declSpecTy
       case initr of
         Nothing -> return()
         Just initr' -> checkInitializer ty initr'
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
            CPtrDeclr _ _ -> return (Just (Ptr ty))
            CArrDeclr _ _ _ -> return (Just (Arr ty))
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
             modifySymTab (SymTab.bindVariable "__func__" (Type.Ptr Type.one))
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

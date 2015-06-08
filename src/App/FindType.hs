{-# LANGUAGE FlexibleInstances #-}

module App.FindType where

import App.FindUnit
import App.Monad.Analysis
import qualified App.SymTab as SymTab
import qualified App.Type as Type
import qualified App.Unit as Unit

import Control.Monad
import Language.C.Pretty
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Syntax.AST

import App.Type
import App.SymTab

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
                         CAssignOp -> if t1 == t2 then return (Just t1)
                                      else do err expr ("Can't assign from " ++ show t2 ++ " to " ++ show t1)
                                              return Nothing
                         _ -> do err expr ("TODO findType CAssign " ++ show op)
                                 return Nothing
                   _ -> return Nothing
          CCond e1 (Just e2) e3 _ -> err expr "TODO findType CCond" >> return Nothing
          CCond e1 Nothing e3 _ -> err expr "TODO findType CCond" >> return Nothing
          CBinary op e1 e2 _ ->
              do t1 <- findType e1
                 t2 <- findType e2
                 case op of
                   CMulOp -> combineTypes expr "can't be multiplied" Type.mul t1 t2
                   CDivOp -> combineTypes expr "can't be divided" Type.div t1 t2
                   CNeqOp -> do _ <- combineTypes expr "can't be compared" Type.add t1 t2
                                return (Just (Numeric (Just Unit.one)))
                   CAddOp -> combineTypes expr "can't be added" Type.add t1 t2
                   _ -> do err expr ("TODO findType CBinary " ++ show op)
                           return Nothing
          CCast (CDecl specs [] _) e _ -> do td <- findType specs
                                             te <- findType e
                                             return (fmap Type.monomorphize td)
          CCast (CDecl specs _ _) e _ -> err expr "TODO findType CCast with triplets" >> return Nothing
          CUnary op e _ ->
              do t <- findType e
                 case op of
                   CPreIncOp -> combineTypes expr "can't be added" Type.add t (Just Type.one)
                   CPreDecOp -> combineTypes expr "can't be subtracted" Type.add t (Just Type.one)
                   CPostIncOp -> combineTypes expr "can't be added" Type.add t (Just Type.one)
                   CPostDecOp -> combineTypes expr "can't be subtracted" Type.add t (Just Type.one)
                   CAdrOp -> err expr "TODO findType CAdrOp" >> return Nothing
                   CIndOp -> err expr "TODO findType CIndOp" >> return Nothing
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
                                                            case lookupField field fields of
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

checkArgs :: NodeInfo -> [Maybe Type] -> [Type] -> Bool -> Analysis ()
checkArgs node actuals formals acceptVarArgs =
    case (actuals, formals, acceptVarArgs) of
      ([], [], _) -> return ()
      ([], _, _) -> err node "Too few args"
      (_, [], True) -> return ()
      (_, [], False) -> err node "Too many args"
      (Nothing : as, f : fs, _) ->
          checkArgs node as fs acceptVarArgs
      (Just a : as, f : fs, _) ->
          do if a /= f then
                 err node ("Argument type mismatch. Found " ++ show a ++ ", expected " ++ show f ++ ".")
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
          CWhile e b _ _ -> err stat "TODO findType CWhile" >> return Nothing
          CFor init cond incr b _ -> err stat "TODO findType CFor" >> return Nothing
          CGoto _ _ -> err stat "TODO findType CGoto" >> return Nothing
          CGotoPtr e _ -> err stat "TODO findType CGotoPtr" >> return Nothing
          CCont _ -> err stat "TODO findType CCont" >> return Nothing
          CBreak _ -> err stat "TODO findType CBreak" >> return Nothing
          CReturn e _ ->
              do ty <- case e of
                         Nothing -> return (Just Other)
                         Just e' -> findType e'
                 st <- getSymTab
                 case returnType st of
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
                    setSymTab (bindTag name (SymTab.variables fieldSymTab) parent)
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
             modifySymTab (bindVariable name (Numeric (Just Unit.one)))

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

applyTriplet :: Pos a => a -> Maybe Type -> Bool -> Triplet -> Analysis ()
applyTriplet node declSpecTy isTypeDef (declr, initr, bitFieldSize) =
    do ty <- case declr of
               Just (CDeclr _ derivedDeclrs _ attrs _) ->
                   do attrType <- findType attrs
                      deriveType derivedDeclrs (Type.mergeMaybe declSpecTy attrType)
               Nothing -> return declSpecTy
       case initr of
         Just (CInitExpr e _) ->
             do initType <- findType e
                case (ty, initType) of
                  (Just ty', Just initType') ->
                      if initType' /= ty' then
                          err e ("Can't assign from " ++ show initType' ++ " to " ++ show ty')
                      else
                          return ()
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
         _ -> err node ("Unhandled CDeclr: " ++ show declr)

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
            CFunDeclr (Left idents) attrs _ ->
                do err d "TODO deriveType1 CFunDeclr with old-style args"
                   return Nothing
            CFunDeclr (Right (cdecls, dots)) attrs _ ->
                do maybeArgs <- mapM argType cdecls
                   case sequence maybeArgs of -- maybe monad
                     Nothing -> return Nothing
                     Just args -> return (Just (Fun ty args dots))

argType :: CDecl -> Analysis (Maybe Type)
argType cdecl =
    case cdecl of
      CDecl specs [] _ -> findType specs
      CDecl specs [(Just (CDeclr _ derivedDeclrs _ attrs _), Nothing, Nothing)] _ ->
          do specType <- findType specs
             attrType <- findType attrs
             deriveType derivedDeclrs (Type.mergeMaybe specType attrType)
      _ -> do err cdecl ("Strange argument:"  ++ show (pretty cdecl))
              return Nothing

funDefArgNames :: CFunDef -> Analysis (Maybe [String])
funDefArgNames f =
    case f of
      CFunDef _ (CDeclr _ derivedDeclrs _ _ _) _ _ _ ->
          case reverse (derivedDeclrs) of
            [] -> do err f "Fundef without derived declarator??"
                     return Nothing
            lastDerivedDeclr : _ ->
                case lastDerivedDeclr of
                  CFunDeclr (Left idents) _ _ ->
                      do err f "TODO argNames CFunDeclr with old-style args"
                         return Nothing
                  CFunDeclr (Right (cdecls, _)) _ _->
                      do maybeNames <- mapM argName cdecls
                         return (sequence maybeNames) -- list monad
                  _ -> do err f "TODO Fundef without function declarator?"
                          return Nothing

argName :: CDecl -> Analysis (Maybe String)
argName cdecl =
    case cdecl of
      CDecl _ [(Just (CDeclr (Just (Ident name _ _)) _ _ _ _), _, _)] _ -> return (Just name)
      _ -> do err cdecl "Can't find name of argument"
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
               Just (Fun rt types _) ->
                   do argNames <- funDefArgNames f
                      modifySymTab (SymTab.setReturnType (Just rt))
                      case argNames of
                        Nothing -> return ()
                        Just names ->
                            if length names /= length types then
                                err f "Number of types and names of args different??"
                            else
                                modifySymTab (SymTab.bindVariables (zip names types))
               _ -> return ()

             _ <- findType body

             setSymTab outsideScope

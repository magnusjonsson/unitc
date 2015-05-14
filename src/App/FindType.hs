{-# LANGUAGE FlexibleInstances #-}

module App.FindType where

import App.FindUnit
import App.Monad.Analysis
import App.SymTab as SymTab
import App.Type as Type
import App.Unit

import Control.Monad
import Language.C.Pretty
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Syntax.AST

import App.Type
import App.SymTab

class FindType a where
    findType :: SymTab -> a -> Analysis (Maybe Type)

instance FindType CExpr where
    findType st expr =
        case expr of
          CComma es _ -> liftM last (mapM (findType st) es)
          CAssign op e1 e2 _ ->
              do mt1 <- findType st e1
                 mt2 <- findType st e2
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
              do mt1 <- findType st e1
                 mt2 <- findType st e2
                 case (mt1, mt2) of
                   (Nothing, Nothing) -> return Nothing
                   (Just t1, Just t2) ->
                       case op of
                         CMulOp -> return (Type.mul t1 t2)
                         CDivOp -> return (Type.div t1 t2)
                         CNeqOp -> do (if t1 /= t2 then
                                         err expr ("comparison of incompatible types: "
                                                   ++ show t1 ++ ", " ++ show t2)
                                       else
                                         return ())
                                      return (Just (Numeric Nothing))
                         _ -> do err expr ("TODO findType CBinary " ++ show op)
                                 return Nothing
                   _ -> do err expr "Missing unit on one side of binary operator"
                           return Nothing
          CCast (CDecl specs [] _) e _ -> do td <- findType st specs
                                             te <- findType st e
                                             return td
          CUnary op e _ -> err expr "TODO findType CUnary" >> return Nothing
          CSizeofExpr e _ -> err expr "TODO findType CSizeofExpr" >> return Nothing
          CSizeofType decl _ -> err expr "TODO findType CSizeofType" >> return Nothing
          CAlignofExpr e _ -> err expr "TODO findType CAlignofExpr" >> return Nothing
          CAlignofType decl _ -> err expr "TODO findType CAlignofType" >> return Nothing
          CComplexReal e _ -> err expr "TODO findType CComplexReal" >> return Nothing
          CComplexImag e _ -> err expr "TODO findType CComplexImag" >> return Nothing
          CIndex e1 e2 _ -> do t1 <- findType st e1
                               t2 <- findType st e1
                               return t1
          CCall e1 es _ ->
              do t1 <- findType st e1
                 actuals <- mapM (findType st) es
                 case t1 of
                   Nothing -> return Nothing
                   Just (Fun rt formals acceptsVarArgs) ->
                       do checkArgs (nodeInfo expr) actuals formals acceptsVarArgs
                          return (Just rt)
                   Just _ ->
                       do err expr ("Non-function called as a function: " ++ show (pretty e1))
                          return Nothing
          CMember e ident bool _ -> err expr "TODO findType CMember" >> return Nothing
          CVar (Ident name _ _) _ -> case SymTab.lookupVariable name st of
                                       Nothing -> do err expr ("Variable not in scope: " ++ name)
                                                     return Nothing
                                       Just ty -> return (Just ty)
          CConst c -> findType st c
          CCompoundLit decl initList _ -> err expr "TODO findType CCompoundLit" >> return Nothing
          CStatExpr stat _ -> err expr "TODO findType CStatExpr" >> return Nothing
          CLabAddrExpr ident _ -> err expr "TODO findType CLabAddrExpr" >> return Nothing
          CBuiltinExpr builtin -> err expr "TODO findType CBuiltinExpr" >> return Nothing

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
    findType st c =
        case c of
          CIntConst _ _ -> return (Just (Numeric Nothing))
          CCharConst _ _ -> return (Just (Numeric Nothing))
          CFloatConst _ _ -> return (Just (Numeric Nothing))
          CStrConst _ _ -> return (Just (Numeric Nothing))

instance FindType CStat where
    findType st stat =
        case stat of
          CLabel _ _ _ _ -> err stat "TODO findType CLabel" >> return Nothing
          CCase e b _ -> err stat "TODO findType CCase" >> return Nothing
          CCases e1 e2 b _ -> err stat "TODO findType CCases" >> return Nothing
          CDefault b _ -> err stat "TODO findType CDefault" >> return Nothing
          CExpr Nothing _ -> err stat "TODO findType CExpr" >> return Nothing
          CExpr (Just e) _ -> findType st e
          CCompound _ blockItems _ -> blockType st Nothing blockItems
          CIf e s1 Nothing _ -> do te <- findType st e
                                   t1 <- findType st s1
                                   return Nothing
          CIf e s1 (Just s2) _ -> do te <- findType st e
                                     t1 <- findType st s1
                                     t1 <- findType st s2
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
                         Just e' -> findType st e'
                 case returnType st of
                   Nothing -> err stat "Encountered return statement but not sure what return type is expected!"
                   Just r ->
                       if ty /= Just r then
                           err stat ("Type " ++ show ty ++ " does not agree with return type " ++ show r)
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
          CTypeDef (Ident name _ _) _ ->
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

blockType :: SymTab -> Maybe Type -> [CBlockItem] -> Analysis (Maybe Type)
blockType st lastType [] = return lastType
blockType st lastType (s : r) =
    do (t, st') <- blockItemTypeAndModifiedSymTab st s
       blockType st' t r

blockItemTypeAndModifiedSymTab :: SymTab -> CBlockItem -> Analysis (Maybe Type, SymTab)
blockItemTypeAndModifiedSymTab st item =
    case item of
      CBlockStmt stmt -> do t <- findType st stmt
                            return (t, st)
      CBlockDecl decl -> do st' <- applyDecl st decl
                            return (Nothing, st')
      CNestedFunDef f -> do err f "TODO findType CNestedFunDef"
                            return (Nothing, st)

applyDecl :: SymTab -> CDecl -> Analysis SymTab
applyDecl st decl =
    case decl of
      CDecl declSpecs triplets _ ->
        applyTriplets decl st declSpecs triplets

type Triplet = (Maybe CDeclr, Maybe CInit, Maybe CExpr)
                           
applyTriplets :: Pos a => a -> SymTab -> [CDeclSpec] -> [Triplet] -> Analysis SymTab
applyTriplets node st declSpecs triplets =
    case triplets of
      [] -> return st
      (triplet : r) -> do st' <- applyTriplet node st declSpecs triplet
                          applyTriplets node st' declSpecs r

isTypeDef :: [CDeclSpec] -> Bool
isTypeDef =
  any (\ spec ->
        case spec of
         (CStorageSpec (CTypedef _)) -> True
         _ -> False)

applyTriplet :: Pos a => a -> SymTab -> [CDeclSpec] -> Triplet -> Analysis SymTab
applyTriplet node st declSpecs (declr, initr, bitFieldSize) =
    do ty <- case declr of
               Just (CDeclr _ derivedDeclrs _ attrs _) ->
                   do specType <- findType st declSpecs
                      attrType <- findType st attrs
                      deriveType st derivedDeclrs (Type.mergeMaybe specType attrType)
               Nothing -> findType st declSpecs
       case initr of
         Just (CInitExpr e _) ->
             do initType <- findType st e
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
                  if isTypeDef declSpecs then
                    return (SymTab.bindType name ty' st)
                  else
                    return (SymTab.bindVariable name ty' st)
                Nothing -> do err declr' ("Could not infer type for " ++ name)
                              return st
         _ -> do err node ("Unhandled CDeclr: " ++ show declr)
                 return st

deriveType :: SymTab -> [CDerivedDeclr] -> Maybe Type -> Analysis (Maybe Type)
deriveType st ds ty =
    case ds of
      [] -> return ty
      (d : dr) -> do ty' <- deriveType st dr ty
                     deriveType1 st d ty'

deriveType1 :: SymTab -> CDerivedDeclr -> Maybe Type -> Analysis (Maybe Type)
deriveType1 st d maybeTy =
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
                do maybeArgs <- mapM (argType st) cdecls
                   case sequence maybeArgs of -- maybe monad
                     Nothing -> return Nothing
                     Just args -> return (Just (Fun ty args dots))

argType :: SymTab -> CDecl -> Analysis (Maybe Type)
argType st cdecl =
    case cdecl of
      CDecl specs [(Just (CDeclr _ derivedDeclrs _ attrs _), Nothing, Nothing)] _ ->
          do specType <- findType st specs
             attrType <- findType st attrs
             deriveType st derivedDeclrs (Type.mergeMaybe specType attrType)
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

argName :: CDecl ->Analysis (Maybe String)
argName cdecl =
    case cdecl of
      CDecl _ [(Just (CDeclr (Just (Ident name _ _)) _ _ _ _), _, _)] _ -> return (Just name)
      _ -> do err cdecl "Can't find name of argument"
              return Nothing

applyFunDef :: SymTab -> CFunDef -> Analysis SymTab
applyFunDef st f =
    case f of
      CFunDef specs (CDeclr ident derivedDeclrs _ attrs _) argDecls body _ ->
          do specType <- findType st specs
             attrType <- findType st attrs
             ty <- deriveType st derivedDeclrs (Type.mergeMaybe specType attrType)
             st' <-
                 case (ident, ty) of
                   (Just (Ident name _ _), Just ty') ->
                       return (SymTab.bindVariable name ty' st)
                   (Nothing, _) -> do err f "Strange fundef! Function has no name!"
                                      return st
                   (_, Nothing) -> do err f "Could not determine function type"
                                      return st
             st'' <-
                 case ty of
                   Just (Fun rt types _) ->
                       do argNames <- funDefArgNames f
                          case argNames of
                            Nothing -> return st'
                            Just names ->
                                if length names /= length types then
                                    do err f "Number of types and names of args different??"
                                       return st'
                                else
                                    return (SymTab.setReturnType (Just rt) $
                                            SymTab.bindVariables (zip names types) $ st')
                   _ -> return st'
             _ <- findType st'' body
             return st'

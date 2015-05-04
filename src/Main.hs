import App.FindType as FindType
import App.FindUnit as FindUnit
import App.SymTab as SymTab
import App.Type as Type
import App.Unit as Unit
import Control.Monad
import Control.Monad.Trans.State.Strict
import Language.C as C
import Language.C.Analysis.DefTable
import Language.C.Analysis.TravMonad
import Language.C.Data.Ident
import Language.C.Syntax.AST
import Language.C.System.GCC as GCC
import System.Environment (getArgs)

main :: IO ()
main = do
  [cFile] <- getArgs
  res <- C.parseCFile (newGCC "gcc") Nothing [] cFile
  case res of
    Left error -> print error
    Right u -> analyzeCTranslUnit u

analyzeCTranslUnit :: CTranslUnit -> IO ()
analyzeCTranslUnit (CTranslUnit decls _) =
    forM_ decls analyzeCExtDecl 

analyzeCExtDecl :: CExtDecl -> IO ()
analyzeCExtDecl extDecl =
    case extDecl of
      CDeclExt d -> analyzeCDecl d
      CFDefExt f -> analyzeCFunDef f
      CAsmExt _ _ -> return ()
                     
analyzeCDecl :: CDecl -> IO ()
analyzeCDecl d = do
  putStrLn "CDecl:"
  print (pretty d)           
  let (CDecl specs triplets _) = d
  putStrLn "specs:"
  print specs
  putStrLn "unit:"
  unit <- findUnit specs
  putStrLn "triplets"
  forM_ triplets (analyzeCDeclTriplet unit)

analyzeCDeclTriplet :: Maybe Unit -> (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> IO ()
analyzeCDeclTriplet defaultUnit r = do
  putStrLn "DeclTriplet"
  let (declr, init, expr) = r
  putStrLn "declr:"
  print declr
  declrUnit <- findUnit declr
  putStrLn "declr unit:"
  print declrUnit
  putStrLn "combined unit:"
  combinedUnit <- findUnit [defaultUnit, declrUnit]
  print combinedUnit
  putStrLn "init:"
  print init
  case init of
    Nothing -> return ()
    Just init' -> analyzeCInit init'
  putStrLn "expr:"
  print expr
      
analyzeCFunDef :: CFunDef -> IO ()
analyzeCFunDef f = do
    putStrLn "Analyzing FunDecl:"
    print (pretty f)
    let CFunDef specs declr decl body _ = f
    putStrLn "specs:"
    print specs
    putStrLn "declr:"
    print (pretty declr)
    putStrLn "decl:"
    print decl
    putStrLn "body:"
    analyzeCStat body

analyzeCStat :: CStat -> IO ()
analyzeCStat stat =
    case stat of
      CLabel _ _ _ _ -> return ()
      CCase e b _ -> analyzeCExpr e >> analyzeCStat b
      CCases e1 e2 b _ -> analyzeCExpr e1 >> analyzeCExpr e2 >> analyzeCStat b
      CDefault b _ -> analyzeCStat b
      CExpr Nothing _ -> return ()
      CExpr (Just e) _ -> analyzeCExpr e
      CCompound _ blockItems _ -> forM_ blockItems analyzeCBlockItem
      CIf e1 s1 Nothing _ -> analyzeCExpr e1 >> analyzeCStat s1
      CIf e1 s1 (Just s2) _ -> analyzeCExpr e1 >> analyzeCStat s1 >> analyzeCStat s2
      CSwitch e b _ -> analyzeCExpr e >> analyzeCStat b
      CWhile e b _ _ -> analyzeCExpr e >> analyzeCStat b
      CFor init cond incr b _ -> do
          case init of
            Left Nothing -> return ()
            Left (Just e) -> analyzeCExpr e
            Right d -> analyzeCDecl d
          case cond of
            Nothing -> return ()
            Just e -> analyzeCExpr e
          case incr of
            Nothing -> return ()
            Just e -> analyzeCExpr e
          analyzeCStat b
      CGoto _ _ -> return ()
      CGotoPtr e _ -> analyzeCExpr e
      CCont _ -> return ()
      CBreak _ -> return ()
      CReturn Nothing _ -> return ()
      CReturn (Just e) _ -> analyzeCExpr e

analyzeCBlockItem :: CBlockItem -> IO ()
analyzeCBlockItem item =
    case item of
      CBlockStmt s -> analyzeCStat s
      CBlockDecl d -> analyzeCDecl d
      CNestedFunDef f -> analyzeCFunDef f

analyzeCExpr :: CExpr -> IO ()
analyzeCExpr expr =
    do putStrLn "Expr:"
       print (pretty expr)
       putStrLn "Type:"
       typ <- findType SymTab.empty expr
       print typ

analyzeCInitListItem :: ([CDesignator], CInit) -> IO ()
analyzeCInitListItem (desigs, init) =
    analyzeCInit init
                            
analyzeCInit :: CInit -> IO ()
analyzeCInit init =
    case init of
      CInitExpr e _ -> analyzeCExpr e
      CInitList inits _ -> forM_ inits analyzeCInitListItem

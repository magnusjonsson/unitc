import App.FindType as FindType
import App.SymTab as SymTab
import App.Monad.Analysis
import Control.Monad
import Language.C as C
import Language.C.Analysis.DefTable
import Language.C.Analysis.TravMonad
import Language.C.Data.Ident
import Language.C.Syntax.AST
import Language.C.System.GCC as GCC
import System.IO
import System.Environment (getArgs)
import System.Exit

main :: IO ()
main = do
  [cFile] <- getArgs
  res <- C.parseCFile (newGCC "gcc") Nothing [] cFile
  case res of
    Left error -> print error
    Right u ->
      let errors = execAnalysis (analyzeCTranslUnit u)
      in do mapM_ printError errors
            case errors of
             [] -> exitSuccess
             _ -> exitFailure

printError :: Err -> IO ()
printError (Err pos msg) =
  hPutStrLn stderr (show pos ++ ": " ++ msg)
               
analyzeCTranslUnit :: CTranslUnit -> Analysis ()
analyzeCTranslUnit (CTranslUnit decls _) =
    forM_ decls analyzeCExtDecl 

analyzeCExtDecl :: CExtDecl -> Analysis ()
analyzeCExtDecl extDecl =
    case extDecl of
      CDeclExt d -> analyzeCDecl d
      CFDefExt f -> analyzeCFunDef f
      CAsmExt _ _ -> return ()
                     
analyzeCDecl :: CDecl -> Analysis ()
analyzeCDecl d = do
  st <- getSymTab
  st' <- applyDecl st d
  setSymTab st'

analyzeCFunDef :: CFunDef -> Analysis ()
analyzeCFunDef f =
    do st <- getSymTab
       st' <- applyFunDef st f
       setSymTab st'

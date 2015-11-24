import FindType
import Monad.Analysis
import SymTab
import Type
import Control.Monad
import Language.C as C
import Language.C.System.GCC as GCC
import Language.C.System.Preprocess as Preprocess
import System.IO
import System.Environment (getArgs)
import System.Exit

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = filter (/= "-g3") rawArgs
  let cpp = newGCC "gcc"
  let (Right (cppArgs, _ignoredArgs)) = parseCPPArgs cpp args
  Right inputStream <- runPreprocessor cpp cppArgs
  let ns = newNameSupply
  let res = execParser translUnitP inputStream (initPos "") builtinTypeNames ns
  case res of
    Left error -> print error
    Right (u, _ns') ->
      let errors = execAnalysis (addGccBuiltins >> analyzeCTranslUnit u)
      in do mapM_ printError errors
            case errors of
             [] -> exitSuccess
             _ -> exitFailure

addGccBuiltins :: Analysis ()
addGccBuiltins =
    do modifySymTab (SymTab.bindType "__builtin_va_list" Type.VaList)
       modifySymTab (SymTab.bindVariable "__builtin_bswap32" (Type.Fun Type.one [(Nothing, Type.one)] False))
       modifySymTab (SymTab.bindVariable "__builtin_bswap64" (Type.Fun Type.one [(Nothing, Type.one)] False))
       modifySymTab (SymTab.bindVariable "__builtin_constant_p" (Type.Fun Type.one [(Nothing, Type.Any)] False))
       modifySymTab (SymTab.bindVariable "__builtin_strchr" (Type.Fun (Type.Ptr Type.one) [(Nothing, Type.Ptr Type.one), (Nothing, Type.one)] False))
       modifySymTab (SymTab.bindVariable "__builtin_expect" (Type.Fun Type.one [(Nothing, Type.one), (Nothing, Type.one)] False))
       modifySymTab (SymTab.bindVariable "__builtin_strlen" (Type.Fun Type.one [(Nothing, Type.Ptr Type.one)] False))
       modifySymTab (SymTab.bindVariable "__builtin_strcmp" (Type.Fun Type.one [(Nothing, Type.Ptr Type.one), (Nothing, Type.Ptr Type.one)] False))
       modifySymTab (SymTab.bindVariable "__builtin_va_start" (Type.Fun Type.Void [(Nothing, Type.VaList), (Nothing, Type.Any)] False))
       modifySymTab (SymTab.bindVariable "__builtin_va_end" (Type.Fun Type.Void [(Nothing, Type.VaList)] False))
       modifySymTab (SymTab.bindVariable "__builtin_strcpy" (Type.Fun (Type.Ptr Type.one) [(Nothing, Type.Ptr Type.one), (Nothing, Type.Ptr Type.one)] False))
       modifySymTab (SymTab.bindVariable "__builtin_strncpy" (Type.Fun (Type.Ptr Type.one) [(Nothing, Type.Ptr Type.one), (Nothing, Type.Ptr Type.one), (Nothing, Type.one)] False))
       modifySymTab (SymTab.bindVariable "__builtin_ctzl" (Type.Fun Type.one [(Nothing, Type.one)] False))

printError :: Err -> IO ()
printError (Err pos msg) =
  hPutStrLn stderr (posFile pos ++ ":" ++ show (posRow pos) ++ ": " ++ msg)
               
analyzeCTranslUnit :: CTranslUnit -> Analysis ()
analyzeCTranslUnit (CTranslUnit decls _) =
    forM_ decls analyzeCExtDecl 

analyzeCExtDecl :: CExtDecl -> Analysis ()
analyzeCExtDecl extDecl =
    case extDecl of
      CDeclExt d -> applyCDecl d
      CFDefExt f -> applyCFunDef f
      CAsmExt _ _ -> return ()

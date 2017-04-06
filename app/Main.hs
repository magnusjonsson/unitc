import FindType
import Monad.Analysis
import SymTab
import Type
import Control.Monad
import Control.Exception (catch, IOException)
import Language.C as C
import Language.C.System.GCC as GCC
import Language.C.System.Preprocess as Preprocess
import System.IO
import System.Environment (getArgs, getEnv)
import System.Exit
import Data.List (isPrefixOf)

argOk :: String -> Bool
argOk arg =
  not ("-g" `isPrefixOf` arg) &&
  not ("-f" `isPrefixOf` arg)

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

main :: IO ()
main = do
  rawArgs <- getArgs
  gccExecutable <- getEnv "UNITC_GCC" `catchIO` (\_ -> return "gcc")
  let args = filter argOk rawArgs
  let cpp = newGCC gccExecutable
  case parseCPPArgs cpp args of
    Left error -> print error >> exitFailure
    Right (cppArgs, _ignoredArgs) ->
      do ppResult <- runPreprocessor cpp cppArgs
         case ppResult of
           Left error -> print error >> exitFailure
           Right inputStream ->
             do let ns = newNameSupply
                let res = execParser translUnitP inputStream (initPos "") builtinTypeNames ns
                case res of
                  Left error -> print error >> exitFailure
                  Right (u, _ns') ->
                    let errors = execAnalysis (addGccBuiltins >> analyzeCTranslUnit u)
                    in do mapM_ printError errors
                          case errors of
                            [] -> exitSuccess
                            _ -> exitFailure

addGccBuiltins :: Analysis ()
addGccBuiltins =
    do modifySymTab (SymTab.bindType "__builtin_va_list" VaList)
       modifySymTab (SymTab.bindVariable "__builtin_bswap32" (Fun one [one] False))
       modifySymTab (SymTab.bindVariable "__builtin_bswap64" (Fun one [one] False))
       modifySymTab (SymTab.bindVariable "__builtin_constant_p" (Fun one [Any] False))
       modifySymTab (SymTab.bindVariable "__builtin_strchr" (Fun (Ptr one) [Ptr one, one] False))
       modifySymTab (SymTab.bindVariable "__builtin_expect" (Fun one [one, one] False))
       modifySymTab (SymTab.bindVariable "__builtin_strlen" (Fun one [Ptr one] False))
       modifySymTab (SymTab.bindVariable "__builtin_strcmp" (Fun one [Ptr one, Ptr one] False))
       modifySymTab (SymTab.bindVariable "__builtin_va_start" (Fun Void [VaList, Any] False))
       modifySymTab (SymTab.bindVariable "__builtin_va_end" (Fun Void [VaList] False))
       modifySymTab (SymTab.bindVariable "__builtin_strcpy" (Fun (Ptr one) [Ptr one, Ptr one] False))
       modifySymTab (SymTab.bindVariable "__builtin_strncpy" (Fun (Ptr one) [Ptr one, Ptr one, one] False))
       modifySymTab (SymTab.bindVariable "__builtin_ctzl" (Fun one [one] False))
       modifySymTab (SymTab.bindVariable "__builtin_prefetch" (Fun Void [Ptr Void] True))

printError :: Err -> IO ()
printError (Err pos msg) =
  hPutStrLn stderr (posFile pos ++ ": " ++ show (posRow pos) ++ ": error: " ++ msg)
               
analyzeCTranslUnit :: CTranslUnit -> Analysis ()
analyzeCTranslUnit (CTranslUnit decls _) =
    forM_ decls analyzeCExtDecl 

analyzeCExtDecl :: CExtDecl -> Analysis ()
analyzeCExtDecl extDecl =
    case extDecl of
      CDeclExt d -> applyCDecl d
      CFDefExt f -> applyCFunDef f
      CAsmExt _ _ -> return ()

import FindType
import Monad.Analysis
import SymTab
import Type
import Control.Monad
import Language.C as C
import Language.C.System.GCC as GCC
import Language.C.System.Preprocess as Preprocess
import System.IO
import System.Environment (getArgs, lookupEnv)
import System.Exit
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B

argOk :: String -> Bool
argOk arg =
  not ("-g" `isPrefixOf` arg) &&
  not ("-f" `isPrefixOf` arg)

main :: IO ()
main = do
  rawArgs <- getArgs
  gccExecutable <- fromMaybe "gcc" `fmap` lookupEnv "UNITC_GCC"
  let args = filter argOk rawArgs
  let cpp = newGCC gccExecutable
  case parseCPPArgs cpp args of
    Left error -> hPutStrLn stderr error >> exitFailure
    Right (cppArgs, _ignoredArgs) ->
      do ppResult <- runPreprocessor cpp cppArgs
         case ppResult of
           Left error -> hPutStrLn stderr ("preprocessor failed with exit code " ++ show error) >> exitFailure
           Right inputStream ->
             let inputStream' = removeHashLines inputStream in
             do let ns = newNameSupply
                let res = execParser translUnitP inputStream' (initPos "") builtinTypeNames ns
                case res of
                  Left error -> hPutStrLn stderr (show error) >> exitFailure
                  Right (u, _ns') ->
                    let errors = execAnalysis (addGccBuiltins >> analyzeCTranslUnit u)
                    in do mapM_ printError errors
                          case errors of
                            [] -> exitSuccess
                            _ -> exitFailure

removeHashLines :: B.ByteString -> B.ByteString
removeHashLines inputStream =
    B.unlines $ filter (\line -> B.length line == 0 || B.head line /= '#') $ B.lines inputStream


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

import App.FindType as FindType
import App.Monad.Analysis
import Control.Monad
import Language.C as C
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
      CDeclExt d -> applyCDecl d
      CFDefExt f -> applyCFunDef f
      CAsmExt _ _ -> return ()

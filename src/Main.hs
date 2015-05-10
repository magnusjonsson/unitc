import App.FindType as FindType
import App.SymTab as SymTab
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
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
    Right u -> evalStateT (analyzeCTranslUnit u) SymTab.empty
               
type Analysis a = StateT SymTab IO a

getSymTab :: Analysis SymTab
getSymTab = get

setSymTab :: SymTab -> Analysis ()
setSymTab = put

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
  st' <- lift (applyDecl st d)
  setSymTab st'

analyzeCFunDef :: CFunDef -> Analysis ()
analyzeCFunDef f =
    do st <- getSymTab
       st' <- lift (applyFunDef st f)
       setSymTab st'

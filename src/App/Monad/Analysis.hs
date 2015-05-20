module App.Monad.Analysis where

import App.SymTab
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class (lift)
import Language.C.Data.Position

data Err = Err Position String

type Analysis a = StateT SymTab (Writer [Err]) a

getSymTab :: Analysis SymTab
getSymTab = get

setSymTab :: SymTab -> Analysis ()
setSymTab = put

modifySymTab :: (SymTab -> SymTab) -> Analysis ()
modifySymTab = modify'

err :: Pos a => a -> String -> Analysis ()
err node msg = do lift (tell [Err (posOf node) msg])

instance Pos Position where
  posOf a = a

execAnalysis :: Analysis () -> [Err]
execAnalysis analysis = execWriter (evalStateT analysis empty)

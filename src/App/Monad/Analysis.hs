module App.Monad.Analysis where

import App.SymTab as SymTab
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class (lift)
import Language.C.Data.Position

data Err = Err Position String

data AnalysisState = AnalysisState {
      symTab :: SymTab,
      genSymCounter :: Int
    }

initialAnalysisState :: AnalysisState
initialAnalysisState =
    AnalysisState { symTab = SymTab.empty,
                    genSymCounter = 0 }

type Analysis a = StateT AnalysisState (Writer [Err]) a

getSymTab :: Analysis SymTab
getSymTab =
    do state <- get
       return (symTab state)

setSymTab :: SymTab -> Analysis ()
setSymTab st =
    do state <- get
       put (state { symTab = st })

modifySymTab :: (SymTab -> SymTab) -> Analysis ()
modifySymTab f =
    modify' (\state -> state { symTab = f (symTab state) })

gensym :: Analysis String
gensym =
    do state <- get
       let counter = genSymCounter state
       put (state { genSymCounter = counter + 1 })
       return ("<anonymous" ++ show counter ++ ">")

err :: Pos a => a -> String -> Analysis ()
err node msg = do lift (tell [Err (posOf node) msg])

instance Pos Position where
  posOf a = a

execAnalysis :: Analysis () -> [Err]
execAnalysis analysis = execWriter (evalStateT analysis initialAnalysisState)

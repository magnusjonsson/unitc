module App.SymTab where

import App.Type as Type
import Data.Map as Map

data SymTab = SymTab {
      variables :: Map String Type,
      returnType :: Maybe Type
    } deriving Show

empty :: SymTab
empty = SymTab { variables = Map.empty, returnType = Nothing }

lookupVariable :: String -> SymTab -> Maybe Type
lookupVariable name symtab =
    Map.lookup name (variables symtab)

bindVariable :: String -> Type -> SymTab -> SymTab
bindVariable name ty st =
    SymTab { variables = Map.insert name ty (variables st),
             returnType = returnType st
           }
setReturnType :: Maybe Type -> SymTab -> SymTab
setReturnType rt st =
    SymTab { variables = variables st,
             returnType = rt
           }

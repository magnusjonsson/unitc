module App.SymTab where

import App.Type as Type
import Data.Map as Map
import Data.List as List

data SymTab = SymTab {
      variables :: Map String Type,
      returnType :: Maybe Type,
      types :: Map String Type
    } deriving Show

empty :: SymTab
empty =
  SymTab { variables = Map.empty,
           returnType = Nothing,
           types = Map.empty }

lookupVariable :: String -> SymTab -> Maybe Type
lookupVariable name symtab =
    Map.lookup name (variables symtab)

bindVariable :: String -> Type -> SymTab -> SymTab
bindVariable name ty st =
  st { variables = Map.insert name ty (variables st) }

bindVariables :: [(String, Type)] -> SymTab -> SymTab
bindVariables pairs st =
  List.foldl' (\acc (name,ty) -> bindVariable name ty acc) st pairs

setReturnType :: Maybe Type -> SymTab -> SymTab
setReturnType rt st =
    st { returnType = rt }

lookupType :: String -> SymTab -> Maybe Type
lookupType name symtab =
    Map.lookup name (types symtab)

bindType :: String -> Type -> SymTab -> SymTab
bindType name ty st =
  st { types = Map.insert name ty (types st) }

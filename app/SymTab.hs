module App.SymTab where

import App.Type as Type
import qualified Data.Map as Map
import Data.List as List
import Control.Monad (mplus)

type Fields = Map.Map String Type

data SymTab = SymTab {
      variables :: Map.Map String Type,
      returnType :: Maybe Type,
      types :: Map.Map String Type,
      tags :: Map.Map String Fields,
      parent :: Maybe SymTab
    } deriving Show

empty :: SymTab
empty =
  SymTab { variables = Map.empty,
           returnType = Nothing,
           types = Map.empty,
           tags = Map.empty,
           parent = Nothing
         }

newScope :: SymTab -> SymTab
newScope p =
    empty { parent = (Just p) }

lookupVariable :: String -> SymTab -> Maybe Type
lookupVariable name symtab =
    Map.lookup name (variables symtab) `mplus`
       do p <- parent symtab
          lookupVariable name p
    

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
    Map.lookup name (types symtab) `mplus`
       do p <- parent symtab
          lookupType name p

bindType :: String -> Type -> SymTab -> SymTab
bindType name ty st =
    st { types = Map.insert name ty (types st) }

lookupTag :: String -> SymTab -> Maybe Fields
lookupTag name symtab =
    Map.lookup name (tags symtab) `mplus`
       do p <- parent symtab
          lookupTag name p

bindTag :: String -> Fields -> SymTab -> SymTab
bindTag name fields st =
    st { tags = Map.insert name fields (tags st) }

lookupField :: String -> Fields -> Maybe Type
lookupField = Map.lookup

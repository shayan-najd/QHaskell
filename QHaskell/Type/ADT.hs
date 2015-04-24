module QHaskell.Type.ADT where

import QHaskell.MyPrelude

data Typ =
    Int
  | Bol
  | Flt
  | Arr Typ Typ
  | Tpl Typ Typ
  | May Typ

deriving instance Eq   Typ
deriving instance Show Typ

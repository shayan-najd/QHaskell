
module QHaskell.Type.ADT where

import QHaskell.MyPrelude
import QHaskell.Nat.ADT

data Typ =
    Wrd
  | Bol
  | Flt
  | Arr Typ Typ
  | Tpl Typ Typ
  | TVr Nat

deriving instance Eq   Typ
deriving instance Show Typ

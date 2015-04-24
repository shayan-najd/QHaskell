module QHaskell.Expression.Utils.Show.GADTFirstOrder
       () where

import QHaskell.MyPrelude
import QHaskell.Expression.GADTFirstOrder as FGFO

deriving instance Show (Exp g a)

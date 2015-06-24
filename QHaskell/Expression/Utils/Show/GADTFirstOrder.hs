module QHaskell.Expression.Utils.Show.GADTFirstOrder
       () where

import QHaskell.MyPrelude
import QHaskell.Expression.GADTFirstOrder as GFO
import QHaskell.Environment.Typed

deriving instance Show (Exp s g a)

instance Show (Env (Exp s g) g') where
  show Emp        = "Emp"
  show (Ext x xs) = "Ext (" ++ show x ++") ("++ show xs ++")"

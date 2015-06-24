module QHaskell.Expression.Utils.Show.Common where

import QHaskell.MyPrelude
-- import qualified QHaskell.Type.GADT as TG
import QHaskell.Environment.Typed hiding (fmap)

data TT a b = TT {unTT :: a}

instance Show (Env (TT String) r') where
    show Emp        = "Emp"
    show (Ext s ss) = "\n Ext (" ++ unTT s ++ ") (" ++ show ss ++ ")"

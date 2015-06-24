module QHaskell.Expression.ADTUntypedDebruijn
       (Exp(..),Fun(..)) where

import QHaskell.MyPrelude
import QHaskell.Variable.Plain
import qualified QHaskell.Type.ADT as TA

data Fun = Fun Exp

deriving instance Eq   Fun
deriving instance Show Fun

data Exp = ConI Word32
         | ConB Bool
         | ConF Float
         | Var  Var
         | Prm  Var [Exp]
         | Abs  Fun
         | App  Exp Exp
         | Cnd  Exp Exp Exp
         | Tpl  Exp Exp
         | Fst  Exp
         | Snd  Exp
         | LeT  Exp Fun
         | Typ  TA.Typ Exp
         | Int  Word32
         | Mem  Exp
         | Fix  Exp

deriving instance Eq   Exp
deriving instance Show Exp

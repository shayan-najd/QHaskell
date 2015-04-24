module QHaskell.Expression.ADTUntypedDebruijn
       (Exp(..),Fun(..)) where

import QHaskell.MyPrelude
import QHaskell.Variable.Plain
import qualified QHaskell.Type.ADT as TFA

data Fun = Fun Exp

deriving instance Eq   Fun
deriving instance Show Fun

data Exp = ConI Int
         | ConB Bool
         | ConF Float
         | Var  Var
         | Abs  Fun
         | App  Exp Exp
         | Tpl  Exp Exp
         | Fst  Exp
         | Snd  Exp
         | Let  Exp Fun
         | Non
         | Som  Exp
         | May  Exp Exp Exp
         | Typ  TFA.Typ Exp
         | Int  Int
         | Mem  Exp

deriving instance Eq   Exp
deriving instance Show Exp

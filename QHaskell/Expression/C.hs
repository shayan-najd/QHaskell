module QHaskell.Expression.C
    (Var,Func(..),Stmt(..),Exp(..))where

import QHaskell.MyPrelude

import qualified QHaskell.Type.ADT as TFA

type Var = (String , TFA.Typ)

data Func = Func TFA.Typ String [Var] [Stmt]

data Stmt =
   If  Exp [Stmt] [Stmt]
 | Whl Exp [Stmt]
 | Assign String Exp
 | Declare Var
 | Return  Exp
 deriving Eq

data Exp =
   Var String
 | Num Int
 | Flt Float
 | App String [Exp]
 deriving Eq

module QHaskell.Expression.ADTUntypedNamed
       (Exp(..)) where

import QHaskell.MyPrelude
import qualified QHaskell.Type.ADT as TA

data Exp x = ConI Word32
           | ConB Bool
           | ConF Float
           | Var x
           | Prm x [Exp x]
           | Abs (x , Exp x)
           | App (Exp x) (Exp x)
           | Cnd (Exp x) (Exp x) (Exp x)
           | Tpl (Exp x) (Exp x)
           | Fst (Exp x)
           | Snd (Exp x)
           | LeT (Exp x) (x , Exp x)
           | Typ TA.Typ (Exp x)
           | Int Word32
           | Mem (Exp x)
           | Fix (Exp x)

deriving instance Eq x   => Eq   (Exp x)
deriving instance Show x => Show (Exp x)
deriving instance Functor     Exp
deriving instance Foldable    Exp
deriving instance Traversable Exp

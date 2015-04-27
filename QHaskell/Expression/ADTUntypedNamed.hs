module QHaskell.Expression.ADTUntypedNamed
       (Exp(..)) where

import QHaskell.MyPrelude
import qualified QHaskell.Type.ADT as TFA

data Exp x = ConI Int
           | ConB Bool
           | ConF Float
           | Var x
           | Abs (x , Exp x)
           | App (Exp x) (Exp x)
           | Tpl (Exp x) (Exp x)
           | Fst (Exp x)
           | Snd (Exp x)
           | Let (Exp x) (x , Exp x)
           | Non
           | Som (Exp x)
           | May (Exp x) (Exp x) (Exp x)
           | Typ TFA.Typ (Exp x)
           | Int Int
           | Mem (Exp x)
           | Fix (Exp x)


deriving instance Eq x   => Eq   (Exp x)
deriving instance Show x => Show (Exp x)
deriving instance Functor     Exp
deriving instance Foldable    Exp
deriving instance Traversable Exp

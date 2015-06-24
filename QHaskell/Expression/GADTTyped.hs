module QHaskell.Expression.GADTTyped
       (Exp(..)) where

import QHaskell.MyPrelude
import QHaskell.Variable.Scoped
import qualified QHaskell.Nat.ADT as NA

data Exp :: NA.Nat -> NA.Nat -> * -> * where
  ConI :: Word32  -> Exp n m t
  ConB :: Bool    -> Exp n m t
  ConF :: Float   -> Exp n m t
  Var  :: Var m   -> Exp n m t
  Prm  :: [t] -> Var n -> [Exp n m t] -> Exp n m t
  Abs  :: Exp n (NA.Suc m) t -> Exp n m t
  App  :: t -> Exp n m t -> Exp n m t -> Exp n m t
  Cnd  :: Exp n m t -> Exp n m t -> Exp n m t -> Exp n m t
  Tpl  :: Exp n m t -> Exp n m t -> Exp n m t
  Fst  :: t -> Exp n m t -> Exp n m t
  Snd  :: t -> Exp n m t -> Exp n m t
  LeT  :: t -> Exp n m t -> Exp n (NA.Suc m) t -> Exp n m t
  Typ  :: t -> Exp n m t -> Exp n m t
  Int  :: Word32 -> Exp n m t
  Mem  :: Exp n m t -> Exp n m t
  Fix  :: Exp n m t -> Exp n m t

deriving instance Eq t   => Eq   (Exp n m t)
deriving instance Show t => Show (Exp n m t)
deriving instance Functor        (Exp n m)
deriving instance Foldable       (Exp n m)
deriving instance Traversable    (Exp n m)

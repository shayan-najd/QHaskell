module QHaskell.Expression.GADTTyped
       (Exp(..)) where

import QHaskell.MyPrelude
import QHaskell.Variable.Scoped
import qualified QHaskell.Nat.ADT as NA

data Exp :: NA.Nat -> * -> * where
  ConI :: Int     -> Exp n t
  ConB :: Bool    -> Exp n t
  ConF :: Float   -> Exp n t
  Var  :: Var n   -> Exp n t
  Abs  :: Exp (NA.Suc n) t -> Exp n t
  App  :: t -> Exp n t -> Exp n t -> Exp n t
  Tpl  :: Exp n t -> Exp n t -> Exp n t
  Fst  :: t -> Exp n t -> Exp n t
  Snd  :: t -> Exp n t -> Exp n t
  Let  :: t -> Exp n t -> Exp (NA.Suc n) t -> Exp n t
  Non  :: Exp n t
  Som  :: Exp n t -> Exp n t
  May  :: t -> Exp n t -> Exp n t -> Exp n t -> Exp n t
  Typ  :: t -> Exp n t -> Exp n t
  Int  :: Int -> Exp n t
  Mem  :: Exp n t -> Exp n t
  Fix  :: Exp n t -> Exp n t



deriving instance Eq t   => Eq   (Exp n t)
deriving instance Show t => Show (Exp n t)
deriving instance Functor        (Exp n)
deriving instance Foldable       (Exp n)
deriving instance Traversable    (Exp n)

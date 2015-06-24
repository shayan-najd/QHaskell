module QHaskell.Expression.GADTFirstOrder
      (Exp(..)) where

import QHaskell.MyPrelude
import QHaskell.Environment.Typed
import QHaskell.Variable.Typed
import QHaskell.Type.GADT
import QHaskell.Magic

data Exp :: [*] -> [*] -> * -> * where
  ConI :: Word32   -> Exp s g Word32
  ConB :: Bool     -> Exp s g Bool
  ConF :: Float    -> Exp s g Float
  Var  :: Var g a  -> Exp s g a
  Prm  :: (Match a as b , Types as) =>
          Var s a -> Env (Exp s g) as -> Exp s g b
  Abs  :: Exp s (a ': g) b -> Exp s g (a -> b)
  App  :: Type a =>
          Exp s g (a -> b) -> Exp s g a -> Exp s g b
  Cnd  :: Exp s g Bool -> Exp s g a -> Exp s g a -> Exp s g a
  Tpl  :: Exp s g a -> Exp s g b -> Exp s g (a , b)
  Fst  :: Type b => Exp s g (a , b) -> Exp s g a
  Snd  :: Type a => Exp s g (a , b) -> Exp s g b
  LeT  :: Type a => Exp s g a -> Exp s (a ': g) b -> Exp s g b
  Int  :: Word32 -> Exp s g a
  Tag  :: String -> Exp s g a -> Exp s g a
  Mem  :: Exp s g a -> Exp s g a
  Fix  :: Exp s g (a -> a) -> Exp s g a

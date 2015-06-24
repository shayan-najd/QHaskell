module QHaskell.Expression.GADTHigherOrder
       (Exp(..)) where

import QHaskell.MyPrelude
import QHaskell.Environment.Typed
import QHaskell.Variable.Typed
import QHaskell.Type.GADT
import QHaskell.Magic

data Exp :: [*] -> * -> * where
  ConI :: Word32   -> Exp s Word32
  ConB :: Bool     -> Exp s Bool
  ConF :: Float    -> Exp s Float
  Prm  :: (Match a as b , Types as) =>
          Var s a -> Env (Exp s) as -> Exp s b
  Abs  :: (Exp s a -> Exp s b) -> Exp s (a -> b)
  App  :: Type a =>
          Exp s (a -> b) -> Exp s a -> Exp s b
  Cnd  :: Exp s Bool -> Exp s a -> Exp s a -> Exp s a
  Tpl  :: Exp s a -> Exp s b -> Exp s (a , b)
  Fst  :: Type b => Exp s (a , b) -> Exp s a
  Snd  :: Type a => Exp s (a , b) -> Exp s b
  LeT  :: Type a =>
          Exp s a -> (Exp s a -> Exp s b) -> Exp s b
  Tmp  :: String -> Exp s a
  Int  :: Word32 -> Exp s a
  Tag  :: String -> Exp s a -> Exp s a
  Mem  :: Exp s a -> Exp s a
  Fix  :: Exp s (a -> a) -> Exp s a

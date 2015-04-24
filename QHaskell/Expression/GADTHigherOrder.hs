module QHaskell.Expression.GADTHigherOrder
       (Exp(..)) where

import QHaskell.MyPrelude
import QHaskell.Variable.Typed
import qualified QHaskell.Type.GADT as TFG
import QHaskell.Singleton

data Exp :: [*] -> * -> * where
  ConI :: Int      -> Exp r Int
  ConB :: Bool     -> Exp r Bol
  ConF :: Float    -> Exp r Flt
  Var  :: Var r t  -> Exp r t
  Abs  :: (Exp r ta -> Exp r tb) -> Exp r (Arr ta tb)
  App  :: HasSin TFG.Typ ta =>
          Exp r (Arr ta tb) -> Exp r ta -> Exp r tb
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (Tpl tf ts)
  Fst  :: HasSin TFG.Typ ts => Exp r (Tpl tf ts) -> Exp r tf
  Snd  :: HasSin TFG.Typ tf => Exp r (Tpl tf ts) -> Exp r ts
  Let  :: HasSin TFG.Typ tl =>
          Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb
  Non  :: Exp r (May tl)
  Som  :: Exp r tl -> Exp r (May tl)
  May  :: HasSin TFG.Typ a =>
          Exp r (May a) -> Exp r b -> Exp r (Arr a b) -> Exp r b
  Tmp  :: String -> Exp r a
  Int  :: Int -> Exp r a
  Tag  :: String -> Exp r t -> Exp r t
  Mem  :: Exp r a -> Exp r a

module QHaskell.Singleton where

-- import QHaskell.MyPrelude
import Prelude hiding (sin)
import QHaskell.ErrorMonad
import Unsafe.Coerce

import qualified QHaskell.Nat.ADT as NA

class HasSin tf t where
  sin :: tf t

-- type family Trm (t :: k) :: *

-- type family RevTrm (t :: *) :: k

data Eql :: k -> k -> * where
  Rfl :: Eql a a

class EqlSin tf where
  eqlSin :: tf t -> tf t' -> ErrM (Eql t t')

data PrfHasSin :: (k -> *) -> k -> *  where
  PrfHasSin :: HasSin tf t => PrfHasSin tf t

class GetPrfHasSin tf where
  getPrfHasSin :: tf t -> PrfHasSin tf t

getPrfHasSinM :: (GetPrfHasSin tf, Monad m) =>
                 tf t -> m (PrfHasSin tf t)
getPrfHasSinM = return . getPrfHasSin

sinTyp :: HasSin tf t => ef t -> tf t
sinTyp _ = sin

sinTypOf :: HasSin tf t => ef t -> tf t' -> tf t
sinTypOf _ _ = sin

samTyp :: tf t -> ef t -> ef t
samTyp _ = id

samTypM :: tf t -> m(ef t) -> m(ef t)
samTypM _ = id

data T t = T

type family Len (l :: [k]) :: NA.Nat where
  Len (x ': xs) = NA.Suc (Len xs)
  Len '[]       = NA.Zro

type family Add (ll :: [k]) (lr :: [k]) :: [k]  where
  Add '[]       lr = lr
  Add (x ': xs) lr = x ': Add xs lr

type a :~: b = Eql a b

obvious :: a :~: b
obvious = unsafeCoerce Rfl

-- Type-level Lookup
type family Lookup (n :: k) (xss :: [(k , k')]) :: Maybe k' where
  Lookup x '[]                 = 'Nothing
  Lookup x ('(x   , a) ': xas) = Just a
  Lookup x ('( x' , a) ': xas) = Lookup x xas

-- type-leve conditional
type family If a b c where
  If True  a b = a
  If False a b = b

-- type-level boolean and operator
type family And a b where
  And True True = True
  And a    b    = False

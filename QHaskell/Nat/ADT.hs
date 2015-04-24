module QHaskell.Nat.ADT
  (Nat(..),prd,inc,add,sub,nat,natStr) where

import QHaskell.MyPrelude

data Nat =
    Zro
  | Suc Nat

deriving instance Eq   Nat
deriving instance Ord  Nat

int :: Nat -> Int
int Zro     = 0
int (Suc x) = 1 + int x

instance Show Nat where
  show v = show (int v)

nat :: Int -> Nat
nat 0 = Zro
nat n = Suc (nat (n - 1))

natStr :: String -> Nat
natStr = nat . read

prd :: Nat -> Nat
prd (Suc n) = n
prd _       = badUse "prd"

inc :: (Nat -> Nat) -> Nat -> Nat
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)

sub :: Nat -> Nat -> Nat
sub n       Zro     = n
sub Zro     _       = badUse "sub"
sub (Suc n) (Suc m) = sub n m

add :: Nat -> Nat -> Nat
add Zro     m = m
add (Suc n) m = Suc (add n m)

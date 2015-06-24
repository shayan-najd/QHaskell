module QHaskell.Expression.ADTValue
    (Exp
    ,conI,conB,conF,prm,var,abs,app,cnd,tpl,fst,snd,leT
    ,typ,int,mem,fix
    ,Lft(..),CoLft(..)) where

import QHaskell.MyPrelude hiding (abs,fst,snd,tpl,cnd,fix)
import qualified QHaskell.MyPrelude as MP
import qualified QHaskell.Type.ADT as TA

data Exp = ConI Word32
         | ConB Bool
         | ConF Float
         | Abs (Exp -> ErrM Exp)
         | Tpl (Exp , Exp)

class Lft t where
  lft :: t -> Exp

instance Lft Exp where
  lft = id

instance Lft Word32 where
  lft = ConI

instance Lft Bool where
  lft = ConB

instance Lft Float where
  lft = ConF

instance (CoLft a , Lft b) => Lft (a -> b) where
  lft f = Abs (fmap (lft . f) . colft)

instance (Lft a , Lft b) => Lft (a , b) where
  lft (x , y) = Tpl (lft x , lft y)

class CoLft t where
  colft :: Exp -> ErrM t

instance CoLft Word32 where
  colft (ConI i) = return i
  colft _        = badTypValM

instance CoLft Bool where
  colft (ConB b) = return b
  colft _        = badTypValM

instance CoLft Float where
  colft (ConF f) = return f
  colft _        = badTypValM

instance (Lft a , CoLft b) => CoLft (a -> ErrM b) where
  colft (Abs f)  = return (\ e -> colft =<< (f (lft e)))
  colft _        = badTypValM

instance (CoLft a , CoLft b) => CoLft (a , b) where
  colft (Tpl (x , y)) = ((,)) <$> colft x <*> colft y
  colft _             = badTypValM

class ToHsk t where
  toHsk :: Exp -> ErrM t

instance ToHsk Word32 where
  toHsk (ConI i) = return i
  toHsk _        = badTypValM

instance ToHsk Bool where
  toHsk (ConB b) = return b
  toHsk _        = badTypValM

instance ToHsk Float where
  toHsk (ConF f) = return f
  toHsk _        = badTypValM

instance ToHsk (Exp -> ErrM Exp) where
  toHsk (Abs f)  = return f
  toHsk _        = badTypValM

instance ToHsk (Exp , Exp) where
  toHsk (Tpl p ) = return p
  toHsk _        = badTypValM

prm :: Exp -> [Exp] -> NamM ErrM Exp
prm f []       = return f
prm f (x : xs) =  do f' <- app f x
                     prm f' xs

prm0 :: Lft a => a -> ErrM Exp
prm0 = return . lft

{-
prm1 :: (ToHsk a , Lft b) => (a -> b) -> Exp -> ErrM Exp
prm1 f x = do x' <- toHsk x
              return (lft (f x'))

prm2 :: (ToHsk a,ToHsk b,Lft c) => (a -> b -> c) -> Exp -> Exp -> ErrM Exp
prm2 f x y = do x' <- toHsk x
                y' <- toHsk y
                return (lft (f x' y'))
-}

var :: a -> NamM ErrM a
var = return

conI :: Word32 -> NamM ErrM Exp
conI = lift . prm0

conB :: Bool -> NamM ErrM Exp
conB = lift . prm0

conF :: Float -> NamM ErrM Exp
conF = lift . prm0

abs :: (Exp -> Exp) -> NamM ErrM Exp
abs f = return (Abs (return . f))

app :: Exp -> Exp -> NamM ErrM Exp
app vf va = do vf' <- lift (toHsk vf)
               lift (vf' va)

cnd :: Exp -> Exp -> Exp -> NamM ErrM Exp
cnd vc v1 v2 = do vc' <- lift (toHsk vc)
                  return (if vc' then v1 else v2)

fst :: Exp -> NamM ErrM Exp
fst (Tpl p) = return (MP.fst p)
fst _       = badTypValM

snd :: Exp -> NamM ErrM Exp
snd (Tpl p) = return (MP.snd p)
snd _       = badTypValM

tpl :: Exp -> Exp -> NamM ErrM Exp
tpl vf vs = return (lft (vf , vs))

leT :: Exp -> (Exp -> Exp) -> NamM ErrM Exp
leT e f = return (f e)

typ :: TA.Typ -> Exp -> NamM ErrM Exp
typ _ = return

int :: Word32 -> NamM ErrM Exp
int = lift . prm0

mem :: Exp -> NamM ErrM Exp
mem = return

fix :: Exp -> NamM ErrM Exp
fix (Abs  l) = lift (fixM l)
fix _        = badTypValM

module Examples.VHDL.Interpreter where

import Prelude
import Control.Applicative
import Data.Function (fix)
import Data.Word
import Data.Maybe

import Examples.VHDL.BackEnd

data Res = Bol Bool
         | Tpl (Res,Res)

type Signal = Word32 -> Res

class Lift a where
  toRes :: a -> Res
  frmRes :: Res -> a

instance Lift Res where
  toRes = id
  frmRes = id

instance Lift Bool where
  toRes = Bol
  frmRes (Bol b) = b
  frmRes _       = error "Type Error!"

instance (Lift a , Lift b) => Lift (a , b) where
  toRes (x , y) = Tpl (toRes x , toRes y)
  frmRes (Tpl (x , y)) = (frmRes x,frmRes y)
  frmRes _             = error "Type Error!"

lift0 :: Lift a => a -> Signal
lift0 b _ = toRes b

lift1 :: (Lift a , Lift b) => (a -> b) -> Signal -> Signal
lift1 f s i = toRes (f (frmRes (s i)))

lift2 :: (Lift a , Lift b , Lift c) =>
         (a -> b -> c) -> Signal -> Signal -> Signal
lift2 f s s' i = toRes (f (frmRes (s i)) (frmRes (s' i)))

{-
class LiftN a where
  type Signals a
  liftN :: a -> Word32 -> Signals a

instance LiftN Bool where
  type Signals Bool = Res
  liftN b _ = toRes b

instance (Lift a , LiftN b) => LiftN (a -> b) where
  type Signals (a -> b) = Signal -> Signals b
  liftN f i = \ s -> liftN (f (frmRes (s i))) i

-}

evl :: [(String,Signal)] -> Exp -> Maybe Signal
evl g l = case l of
  Bool  b   -> pure (lift0 b)
  VarB  x   -> lookup x g
  Inv   m   -> lift1 not                         <$> evl g m
  Fst   m   -> lift1 (fst :: (Res , Res) -> Res) <$> evl g m
  Snd   m   -> lift1 (snd :: (Res , Res) -> Res) <$> evl g m
  And   m n -> lift2 (&&) <$> evl g m <*> evl g n
  Or    m n -> lift2 (||) <$> evl g m <*> evl g n
  Xor   m n -> lift2 (\ x y -> if x then not y else y) <$> evl g m <*> evl g n
  Delay m n -> (\ m' n'  i -> if i == 0 then m' i else n' (i - 1)) <$> evl g m <*> evl g n
  Pair  m n -> lift2 ((,) :: Res -> Res -> (Res,Res))
               <$> evl g m <*> evl g n
  Fix   x n -> fix (\ ~(Just n') -> evl ((x , n') : g) n)
  Let x m n -> do m' <- evl g m
                  evl ((x, m') : g) n

toSignal :: Lift a => [a] -> Signal
toSignal xs = \ i -> toRes (xs !! (fromIntegral i))

simSeq :: Lift a => (Exp -> Exp) -> [a] -> [a]
simSeq f inps = fmap (frmRes .
                  (fromJust (evl [("$input" , toSignal inps)] (f (VarB "$input")))))
                 [0..(fromIntegral (length inps)) -1]

sim :: Lift a => (Exp -> Exp) -> a -> a
sim f inp = head (simSeq f [inp])

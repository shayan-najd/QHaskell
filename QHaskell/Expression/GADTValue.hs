module QHaskell.Expression.GADTValue
    (Exp(..)
    ,conI,conB,conF,prm,var,abs,app,cnd,tpl,fst,snd,leT
    ,tag,int,mem,fix
    ,getTrm) where

import QHaskell.MyPrelude hiding (abs,fst,snd,tpl,cnd,fix)
import qualified QHaskell.MyPrelude as MP
import qualified QHaskell.Type.GADT as TG
import qualified QHaskell.Environment.Typed as ET
import QHaskell.Singleton
import QHaskell.Magic
import Unsafe.Coerce

data Exp :: * -> * where
  Exp :: t -> Exp t

deriving instance Functor Exp

getTrm :: Exp t -> t
getTrm (Exp x) = x

-- The functionunsafeCoerce is "safe" to be used here,
-- since type-safety is guaranteed by Trm datatype.
prm :: forall a as b. Match a as b => Exp a -> ET.Env Exp as -> Exp b
prm f xss = unsafeCoerce(ET.foldl (\ (Exp f') (Exp x) -> Exp ((unsafeCoerce f') x)) f xss)

prm0 :: a -> Exp a
prm0 = Exp

prm1 :: (a -> b) -> Exp a -> Exp b
prm1 f = fmap f

prm2 :: (a -> b -> c) ->
        Exp a -> Exp b -> Exp c
prm2 f e1 e2 = let e1' = getTrm e1
                   e2' = getTrm e2
               in Exp (f e1' e2')

prm3 :: (a -> b -> c -> d) ->
        Exp a -> Exp b -> Exp c -> Exp d
prm3 f e1 e2 e3 = let e1' = getTrm e1
                      e2' = getTrm e2
                      e3' = getTrm e3
                  in  Exp (f e1' e2' e3')

var :: t -> t
var = id

conI :: Word32 -> Exp Word32
conI = prm0

conB :: Bool -> Exp Bool
conB = prm0

conF :: Float -> Exp Float
conF = prm0

abs :: Exp (ta -> tb) -> Exp (ta -> tb)
abs = id

app :: Exp (ta -> tb) -> Exp ta -> Exp tb
app = prm2 (\ f x -> f x)

cnd :: Exp Bool -> Exp a -> Exp a -> Exp a
cnd = prm3 MP.cnd

tpl :: Exp tf -> Exp ts -> Exp (tf , ts)
tpl = prm2 MP.tpl

fst :: Exp (a , b) -> Exp a
fst = prm1 MP.fst

snd :: Exp (a , b) -> Exp b
snd = prm1 MP.snd

leT :: Exp tl -> Exp (tl -> tb) -> Exp tb
leT = prm2 (\ x f -> f x)

tag :: String -> Exp a -> Exp a
tag = const id

int :: forall a. TG.Type a => Word32 -> Exp a
int = case sin :: TG.Typ a of
  TG.Wrd -> Exp . fromIntegral
  TG.Flt -> Exp . fromIntegral
  _      -> badTypVal

mem :: Exp a -> Exp a
mem = id

fix :: Exp (a -> a) -> Exp a
fix = prm1 MP.fix

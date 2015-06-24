{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Type.GADT where

import QHaskell.MyPrelude
import qualified QHaskell.Environment.Typed as ET
import QHaskell.Singleton
import QHaskell.Nat.GADT

data Typ :: * -> * where
  Wrd :: Typ Word32
  Bol :: Typ Bool
  Flt :: Typ Float
  Arr :: Typ ta -> Typ tb -> Typ (ta -> tb)
  Tpl :: Typ tf -> Typ ts -> Typ (tf , ts)
  TVr :: Nat a  -> Typ (TVr a)

deriving instance Show (Typ t)

type Type a = HasSin Typ a
type Types as = HasSin (ET.Env Typ) as

instance HasSin Typ Word32 where
  sin = Wrd

instance HasSin Typ Bool where
  sin = Bol

instance HasSin Typ Float where
  sin = Flt

instance (Type ta , Type tb) => HasSin Typ (ta -> tb) where
  sin = Arr sin sin

instance (Type tf , Type ts) => HasSin Typ (tf , ts) where
  sin = Tpl sin sin

instance (HasSin Nat x) => HasSin Typ (TVr x) where
  sin = TVr sin

instance EqlSin Typ where
  eqlSin Wrd         Wrd           = return Rfl
  eqlSin Bol         Bol           = return Rfl
  eqlSin Flt         Flt           = return Rfl
  eqlSin (Arr ta tb) (Arr ta' tb') = do Rfl <- eqlSin ta ta'
                                        Rfl <- eqlSin tb tb'
                                        return Rfl
  eqlSin (Tpl tf ts) (Tpl tf' ts') = do Rfl <- eqlSin tf tf'
                                        Rfl <- eqlSin ts ts'
                                        return Rfl
  eqlSin (TVr n)     (TVr n')      = do Rfl <- eqlSin n n'
                                        return Rfl
  eqlSin a            a'            = fail ("Type Error!\n"++
                 show a ++ " is not equal to " ++ show a' ++"!")

instance GetPrfHasSin Typ where
  getPrfHasSin t  = case t of
    Wrd       -> PrfHasSin
    Bol       -> PrfHasSin
    Flt       -> PrfHasSin
    Arr ta tb -> case (getPrfHasSin ta , getPrfHasSin tb) of
      (PrfHasSin , PrfHasSin) -> PrfHasSin
    Tpl tf ts -> case (getPrfHasSin tf , getPrfHasSin ts) of
      (PrfHasSin , PrfHasSin) -> PrfHasSin
    TVr n     -> case getPrfHasSin n of
      PrfHasSin -> PrfHasSin

getPrfHasSinArr :: forall ta tb t. Type (ta -> tb) =>
                   t (ta -> tb) -> (PrfHasSin Typ ta , PrfHasSin Typ tb)
getPrfHasSinArr _ = case sin :: Typ (ta -> tb) of
  Arr ta tb -> (getPrfHasSin ta , getPrfHasSin tb)

getPrfHasSinTpl :: forall tf ts t. Type (tf , ts) =>
                   t (tf , ts) -> (PrfHasSin Typ tf , PrfHasSin Typ ts)
getPrfHasSinTpl _ = case sin :: Typ (tf , ts) of
  Tpl tf ts -> (getPrfHasSin tf , getPrfHasSin ts)

getPrfHasSinArrM :: Type (ta -> tb) =>
                    t (ta -> tb) -> ErrM (PrfHasSin Typ ta , PrfHasSin Typ tb)
getPrfHasSinArrM = return . getPrfHasSinArr

getPrfHasSinTplM :: Type (tf , ts) =>
                    t (tf , ts) -> ErrM (PrfHasSin Typ tf , PrfHasSin Typ ts)
getPrfHasSinTplM = return . getPrfHasSinTpl

mapC :: Types as =>
        (forall a. Type a => f a -> f' a) ->
        ET.Env f as -> ET.Env f' as
mapC f xss = case xss of
  ET.Emp      -> ET.Emp
  ET.Ext x xs -> case getPrfHasSinEnvOf xss of
    (PrfHasSin,PrfHasSin) -> ET.Ext (f x) (mapC f xs)

mapMC :: (Applicative m , Types as) =>
         (forall a. Type a => f a -> m (f' a)) ->
         ET.Env f as -> m (ET.Env f' as)
mapMC f xss = case xss of
  ET.Emp      -> pure ET.Emp
  ET.Ext x xs -> case getPrfHasSinEnvOf xss of
    (PrfHasSin,PrfHasSin) -> ET.Ext <$> f x <*> mapMC f xs

fld :: Types as => (forall a. Type a => b -> f a -> b) -> b ->
       ET.Env f as -> b
fld f z xss = case xss of
  ET.Emp      -> z
  ET.Ext e es -> case getPrfHasSinEnvOf xss of
   (PrfHasSin , PrfHasSin) -> f (fld f z es) e

getArgTyp :: Typ (ta -> tb) -> Typ ta
getArgTyp (Arr ta _) = ta

getBdyTyp :: Typ (ta -> tb) -> Typ tb
getBdyTyp (Arr _ tb) = tb

getFstTyp :: Typ (tf , ts) -> Typ tf
getFstTyp (Tpl tf _) = tf

getSndTyp :: Typ (tf , ts) -> Typ ts
getSndTyp (Tpl _  ts) = ts

type family (:->) a b where
  '[]       :-> b = b
  (a ': as) :-> b = a -> (as :-> b)

cur :: ET.Env Typ as -> Typ a -> Typ (as :-> a)
cur ET.Emp        b = b
cur (ET.Ext a as) b = Arr a (cur as b)

getPrfHasSinEnv :: forall a as. Types (a ': as) => (PrfHasSin Typ a, PrfHasSin (ET.Env Typ) as)
getPrfHasSinEnv = case sin :: ET.Env Typ (a ': as) of
  ET.Ext a as  -> (getPrfHasSin a , getPrfHasSin as)

getPrfHasSinEnvOf :: forall a as f. Types (a ': as) =>
                     ET.Env f (a ': as) -> (PrfHasSin Typ a, PrfHasSin (ET.Env Typ) as)
getPrfHasSinEnvOf _ = getPrfHasSinEnv

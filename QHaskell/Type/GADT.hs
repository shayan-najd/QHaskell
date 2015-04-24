module QHaskell.Type.GADT where

import QHaskell.MyPrelude
import qualified QHaskell.Environment.Typed as ET
import QHaskell.Singleton

data Typ :: * -> * where
  Int :: Typ Int
  Bol :: Typ Bol
  Flt :: Typ Flt
  Arr :: Typ ta -> Typ tb -> Typ (ta -> tb)
  Tpl :: Typ tf -> Typ ts -> Typ ((tf , ts))
  May :: Typ t  -> Typ (May t)

deriving instance Show (Typ t)

instance HasSin Typ Int where
  sin = Int

instance HasSin Typ Bol where
  sin = Bol

instance HasSin Typ Flt where
  sin = Flt

instance (HasSin Typ ta , HasSin Typ tb) => HasSin Typ (Arr ta tb) where
  sin = Arr sin sin

instance (HasSin Typ tf , HasSin Typ ts) => HasSin Typ (Tpl tf ts) where
  sin = Tpl sin sin

instance HasSin Typ ta => HasSin Typ (May ta) where
  sin = May sin

instance EqlSin Typ where
  eqlSin Int         Int           = return Rfl
  eqlSin Bol         Bol           = return Rfl
  eqlSin Flt         Flt           = return Rfl
  eqlSin (Arr ta tb) (Arr ta' tb') = do Rfl <- eqlSin ta ta'
                                        Rfl <- eqlSin tb tb'
                                        return Rfl
  eqlSin (Tpl tf ts) (Tpl tf' ts') = do Rfl <- eqlSin tf tf'
                                        Rfl <- eqlSin ts ts'
                                        return Rfl
  eqlSin (May t)     (May t')      = do Rfl <- eqlSin t t'
                                        return Rfl
  eqlSin _              _          = fail "Type Error!"

instance GetPrfHasSin Typ where
  getPrfHasSin t  = case t of
    Int       -> PrfHasSin
    Bol       -> PrfHasSin
    Flt       -> PrfHasSin
    Arr ta tb -> case (getPrfHasSin ta , getPrfHasSin tb) of
      (PrfHasSin , PrfHasSin) -> PrfHasSin
    Tpl tf ts -> case (getPrfHasSin tf , getPrfHasSin ts) of
      (PrfHasSin , PrfHasSin) -> PrfHasSin
    May ta    -> case getPrfHasSin ta of
      PrfHasSin -> PrfHasSin

getPrfHasSinArr :: forall ta tb t. HasSin Typ (Arr ta tb) =>
                   t (Arr ta tb) -> (PrfHasSin Typ ta , PrfHasSin Typ tb)
getPrfHasSinArr _ = case sin :: Typ (Arr ta tb) of
  Arr ta tb -> (getPrfHasSin ta , getPrfHasSin tb)

getPrfHasSinTpl :: forall tf ts t. HasSin Typ (Tpl tf ts) =>
                   t (Tpl tf ts) -> (PrfHasSin Typ tf , PrfHasSin Typ ts)
getPrfHasSinTpl _ = case sin :: Typ (Tpl tf ts) of
  Tpl tf ts -> (getPrfHasSin tf , getPrfHasSin ts)

getPrfHasSinMay :: forall ta t. HasSin Typ (May ta) =>
                   t (May ta) -> PrfHasSin Typ ta
getPrfHasSinMay _ = case sin :: Typ (May ta) of
  May ta    -> getPrfHasSin ta

getPrfHasSinArrM :: HasSin Typ (Arr ta tb) =>
                    t (Arr ta tb) -> ErrM (PrfHasSin Typ ta , PrfHasSin Typ tb)
getPrfHasSinArrM = return . getPrfHasSinArr

getPrfHasSinTplM :: HasSin Typ (Tpl tf ts) =>
                    t (Tpl tf ts) -> ErrM (PrfHasSin Typ tf , PrfHasSin Typ ts)
getPrfHasSinTplM = return . getPrfHasSinTpl

getPrfHasSinMayM :: HasSin Typ (May ta) =>
                   t (May ta) -> ErrM (PrfHasSin Typ ta)
getPrfHasSinMayM = return  . getPrfHasSinMay

type family Out (t :: *) :: * where
  Out (ta -> tb) = Out tb
  Out t          = t

data EqlOut :: * -> * -> * where
  EqlOut :: EqlOut (Out t2) t2

eqlOut :: Typ t1 -> Typ t2 -> ErrM (EqlOut t1 t2)
eqlOut Int         Int           = return EqlOut
eqlOut Bol         Bol           = return EqlOut
eqlOut Flt         Flt           = return EqlOut
eqlOut t           (Arr _ tb)    = do EqlOut <- eqlOut t tb
                                      return EqlOut
eqlOut (May ta)    (May ta')     = do Rfl <- eqlSin ta ta'
                                      return EqlOut
eqlOut (Tpl tf ts) (Tpl tf' ts') = do Rfl <- eqlSin tf tf'
                                      Rfl <- eqlSin ts ts'
                                      return EqlOut
eqlOut _              _          = fail "Normalisation Error!"

type family Arg (t :: *) :: [*] where
  Arg (ta -> tb) = ta ': Arg tb
  Arg t          = '[]

data EqlArg :: [*] -> * -> * where
  EqlArg :: EqlArg (Arg t2) t2

eqlArg :: ET.Env Typ r -> Typ t -> ErrM (EqlArg r t)
eqlArg ET.Emp         Int          = return EqlArg
eqlArg ET.Emp         Bol          = return EqlArg
eqlArg ET.Emp         Flt          = return EqlArg
eqlArg (ET.Ext ta ts) (Arr ta' tb) = do Rfl    <- eqlSin ta ta'
                                        EqlArg <- eqlArg ts tb
                                        return EqlArg
eqlArg ET.Emp         (May _)      = return EqlArg
eqlArg ET.Emp         (Tpl _ _)    = return EqlArg
eqlArg _              _            = fail "Normalisation Error!"

mapC :: Typ tt -> (forall t. HasSin Typ t => tfa t -> tfb t) ->
        ET.Env tfa (Arg tt) -> ET.Env tfb (Arg tt)
mapC _              _ ET.Emp     = ET.Emp
mapC (Arr t ts) f (ET.Ext x xs)  = case getPrfHasSin t of
  PrfHasSin                     -> ET.Ext (f x) (mapC ts f xs)
mapC _              _ _          = impossible

mapMC :: Monad m =>
        Typ tt -> (forall t. HasSin Typ t => tfa t -> m (tfb t)) ->
        ET.Env tfa (Arg tt) -> m (ET.Env tfb (Arg tt))
mapMC _              _ ET.Emp    = return (ET.Emp)
mapMC (Arr t ts) f (ET.Ext x xs) = case getPrfHasSin t of
  PrfHasSin -> do x'  <- f x
                  xs' <- mapMC ts f xs
                  return (ET.Ext x' xs')
mapMC _              _ _        = impossibleM

fld :: (forall t. HasSin Typ t => b -> e t -> b) -> b ->
       Typ tt -> ET.Env e (Arg tt) -> b
fld _ z _              ET.Emp   = z
fld f z (Arr a b) (ET.Ext e es) = case getPrfHasSin a of
    PrfHasSin -> f (fld f z b es) e
fld _ _  _             _        = impossible

getArgTyp :: Typ (Arr ta tb) -> Typ ta
getArgTyp (Arr ta _) = ta

getBdyTyp :: Typ (Arr ta tb) -> Typ tb
getBdyTyp (Arr _ tb) = tb

getFstTyp :: Typ (Tpl tf ts) -> Typ tf
getFstTyp (Tpl tf _) = tf

getSndTyp :: Typ (Tpl tf ts) -> Typ ts
getSndTyp (Tpl _  ts) = ts

getMayTyp :: Typ (May ta) -> Typ ta
getMayTyp (May ta) = ta

prfHasSinOut :: forall t. PrfHasSin Typ t -> PrfHasSin Typ (Out t)
prfHasSinOut PrfHasSin = case sin :: Typ t of
  Int       -> PrfHasSin
  Bol       -> PrfHasSin
  Flt       -> PrfHasSin
  Arr ta tb -> case (getPrfHasSin ta , getPrfHasSin tb) of
    (PrfHasSin , y@PrfHasSin) -> prfHasSinOut y
  Tpl tf ts -> case (getPrfHasSin tf , getPrfHasSin ts) of
    (PrfHasSin , PrfHasSin) -> PrfHasSin
  May ta    -> case getPrfHasSin ta of
    PrfHasSin -> PrfHasSin

getTypTailEnv :: forall t ra rb tf.
                 (Arg t ~ Add ra rb) =>
                 Typ t -> ET.Env tf ra -> ET.Env tf rb -> ExsSin Typ
getTypTailEnv t          ET.Emp        _ = ExsSin t
getTypTailEnv (Arr _ ts) (ET.Ext _ rs) r = getTypTailEnv ts rs r
getTypTailEnv _          _             _ = impossible

getSinDiff :: (Arg t ~ Add ra rb) =>
              Typ t -> ET.Env tf ra -> ET.Env tf rb -> ET.Env Typ rb
getSinDiff _          _             ET.Emp          = ET.Emp
getSinDiff (Arr _ t)  (ET.Ext _ ra) rb@(ET.Ext _ _) = getSinDiff t ra rb
getSinDiff (Arr t ts) ET.Emp        (ET.Ext _ rb)   = ET.Ext t (getSinDiff ts ET.Emp rb)
getSinDiff _          _             _               = impossible

getPrf :: (Arg t ~ Add ra (tb ': rb)) =>
          Typ t -> ET.Env tf ra -> ET.Env tf (tb ': rb) -> PrfHasSin Typ tb
getPrf Int                   _          _           = impossible
getPrf Bol                   _          _           = impossible
getPrf Flt                   _          _           = impossible
getPrf (Tpl _ _)             _          _           = impossible
getPrf (May _)               _          _           = impossible
getPrf (Arr t Int)       ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ Int)       _          _           = impossible
getPrf (Arr t Bol)       ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ Bol)       _          _           = impossible
getPrf (Arr t Flt)       ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ Flt)       _          _           = impossible
getPrf (Arr t (Tpl _ _)) ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ (Tpl _ _)) _          _           = impossible
getPrf (Arr t (May _))   ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ (May _))   _          _           = impossible
getPrf (Arr t (Arr _ _)) ET.Emp        (ET.Ext _ _)   = getPrfHasSin t
getPrf (Arr _ ts@(Arr _ _)) (ET.Ext _ es) es'      = getPrf ts es es'

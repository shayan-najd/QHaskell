module QHaskell.Expression.Utils.Equality.GADTFirstOrder (eql) where

import QHaskell.MyPrelude

import QHaskell.Variable.Typed
import QHaskell.Environment.Typed
import QHaskell.Expression.GADTFirstOrder
import QHaskell.Singleton
import qualified QHaskell.Type.GADT as TG

tt :: TG.Typ a
tt = tt

eql :: forall s g a.  Exp s g a -> Exp s g a -> Bool
eql (ConI i)    (ConI i')     = i == i'
eql (ConI _)    _             = False

eql (ConB b)    (ConB b')     = b == b'
eql (ConB _)    _             = False

eql (ConF f)    (ConF f')     = f == f'
eql (ConF _)    _             = False

eql (Var  v)    (Var  v')     = v == v'
eql (Var  _)    _             = False

eql (Prm x  (ns  :: Env (Exp s g) d))
    (Prm x' (ns' :: Env (Exp s g) d')) = case eqlSin (sinTyp ns  :: Env TG.Typ d)
                                                     (sinTyp ns' :: Env TG.Typ d') of
   Rgt Rfl -> case eqlVar x x' of
               Just _  -> eqlEnv ns ns'
               Nothing -> False
   Lft _   -> False

eql (Prm _ _)   _             = False

eql (Abs  f)    (Abs  f')     = eql f f'
eql (Abs  _)    _             = False

eql (App ef ea) (App ef' ea') =
  case eqlSin (sinTypOf ea tt) (sinTypOf ea' tt) of
    Rgt Rfl -> eql ef ef' && eql ea ea'
    _       -> False
eql (App _ _)   _             = False

eql (Cnd ec et ef) (Cnd ec' et' ef') = eql ec ec' && eql et et'
                                       && eql ef ef'
eql (Cnd _  _  _ ) _                 = False

eql (Tpl ef es)    (Tpl ef' es')     = eql ef ef' && eql es es'
eql (Tpl _  _ )    _                 = False

eql (Fst (e :: Exp s g (t , ts))) (Fst (e' :: Exp s g (t  , ts'))) =
  case eqlSin (sin :: TG.Typ ts) (sin :: TG.Typ ts') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Fst _)     _               = False

eql (Snd (e :: Exp s g (tf , t))) (Snd (e' :: Exp s g (tf' , t))) =
  case eqlSin (sin :: TG.Typ tf) (sin :: TG.Typ tf') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Snd _)     _               = False

eql (LeT el eb) (LeT el' eb') =
  case eqlSin (sinTypOf el tt) (sinTypOf el' tt) of
    Rgt Rfl -> eql el el' && eql eb eb'
    _       -> False
eql (LeT _ _)   _             = False

eql (Int i)     (Int j)       = i == j
eql (Int _)     _             = False

eql (Tag _ e)   (Tag _ e')    = eql e e' -- ignore tags
eql (Tag _ _)   _             = False

eql (Mem e)     (Mem e')      = eql e e'
eql (Mem _)     _             = False

eql (Fix e)     (Fix e')      = eql e e'
eql (Fix _)     _             = False

eqlEnv :: forall d d' s g. (TG.Types d , TG.Types d') =>
          Env (Exp s g) d ->  Env (Exp s g) d' -> Bool
eqlEnv d d' = case eqlSin (sin :: Env TG.Typ d) (sin :: Env TG.Typ d') of
   Rgt Rfl -> case (d , d') of
     (Emp      , Emp)        -> True
     (Ext x xs , Ext x' xs') -> case (TG.getPrfHasSinEnvOf d , TG.getPrfHasSinEnvOf d') of
       ((PrfHasSin , PrfHasSin),(PrfHasSin , PrfHasSin)) -> eql x x' && eqlEnv xs xs'
     _                       -> False
   _                         -> False

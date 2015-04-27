module QHaskell.Expression.Utils.Equality.GADTFirstOrder (eql) where

import QHaskell.MyPrelude

import QHaskell.Expression.GADTFirstOrder
import QHaskell.Singleton
import qualified QHaskell.Type.GADT as TFG

tt :: TFG.Typ a
tt = tt

eql :: forall r t .  Exp r t -> Exp r t -> Bool
eql (ConI i)    (ConI i')     = i == i'
eql (ConI _)    _             = False

eql (ConB b)    (ConB b')     = b == b'
eql (ConB _)    _             = False

eql (ConF f)    (ConF f')     = f == f'
eql (ConF _)    _             = False

eql (Var  v)    (Var  v')     = v == v'
eql (Var  _)    _             = False

eql (Abs  f)    (Abs  f')     = eql f f'
eql (Abs  _)    _             = False

eql (App ef ea) (App ef' ea') =
  case eqlSin (sinTypOf ea tt) (sinTypOf ea' tt) of
    Rgt Rfl -> eql ef ef' && eql ea ea'
    _       -> False
eql (App _ _)   _             = False

eql (Tpl ef es)    (Tpl ef' es')     = eql ef ef' && eql es es'
eql (Tpl _  _ )    _                 = False

eql (Fst (e :: Exp r (Tpl t ts))) (Fst (e' :: Exp r (Tpl t ts'))) =
  case eqlSin (sin :: TFG.Typ ts) (sin :: TFG.Typ ts') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Fst _)     _               = False

eql (Snd (e :: Exp r (Tpl tf t))) (Snd (e' :: Exp r (Tpl tf' t))) =
  case eqlSin (sin :: TFG.Typ tf) (sin :: TFG.Typ tf') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Snd _)     _               = False

eql (Let el eb) (Let el' eb') =
  case eqlSin (sinTypOf el tt) (sinTypOf el' tt) of
    Rgt Rfl -> eql el el' && eql eb eb'
    _       -> False
eql (Let _ _)   _             = False

eql Non         Non           = True
eql Non         _             = False

eql (Som e)     (Som e')      = eql e e'
eql (Som _)     _             = False

eql (May em  en  es ) (May em' en' es') =
  case eqlSin (sinTypOf em tt) (sinTypOf em' tt) of
    Rgt Rfl -> eql em em' && eql en en' && eql es es'
    _       -> False
eql (May _ _ _) _             = False

eql (Int i)     (Int j)       = i == j
eql (Int _)     _             = False

eql (Tag _ e)   (Tag _ e')    = eql e e' -- ignore tags
eql (Tag _ _)   _             = False

eql (Mem e)     (Mem e')      = eql e e'
eql (Mem _)     _             = False

eql (Fix e)     (Fix e')      = eql e e'
eql (Fix _)     _             = False

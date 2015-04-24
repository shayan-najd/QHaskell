{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.Evaluation.GADTTyped () where

import QHaskell.MyPrelude
import QHaskell.Expression.GADTTyped
import qualified QHaskell.Expression.ADTValue as FAV
import QHaskell.Environment.Scoped
import QHaskell.Nat.ADT
import QHaskell.Conversion
import QHaskell.Variable.Conversion ()

instance Cnv (Exp n t , Env n FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of
    Var v        -> pure (pure (get v r))
    App _  ef ea -> FAV.app  <$@> ef <*@> ea
    Fst _  e     -> FAV.fst  <$@> e
    Snd _  e     -> FAV.snd  <$@> e
    Let _  el eb -> FAV.leT  <$@> el <*@> eb
    Typ _ e      -> pure (cnvImp e)
    Non          -> impossibleM
    Som _        -> impossibleM
    May _ _ _ _  -> impossibleM
    _ -> $(biGenOverloadedML 'ee ''Exp "FAV"
     ['Var,'App,'Fst,'Snd,'Let,'Typ,
      'Non,'Som,'May] (const [| cnvImp |])))

instance Cnv (Exp (Suc n) t , Env n FAV.Exp) (FAV.Exp -> FAV.Exp) where
  cnv (e , r) = pure (frmRgtZro . curry cnv e . (flip Ext r))

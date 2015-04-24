{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.Evaluation.ADTUntypedNamed () where

import QHaskell.MyPrelude

import QHaskell.Expression.ADTUntypedNamed
import qualified QHaskell.Expression.ADTValue as FAV

import QHaskell.Environment.Map

import QHaskell.Conversion
import QHaskell.Variable.Conversion ()

instance Eq v => Cnv (Exp v , Env v FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of
    Var  v       -> pure (get v r)
    Non          -> impossibleM
    Som _        -> impossibleM
    May _ _ _    -> impossibleM
    Let el eb    -> FAV.leT  <$@> el <*@> eb
    _ -> $(biGenOverloadedML 'ee ''Exp "FAV"
     ['Var,'Non,'Som,'May,'Let]
     (const [| cnvImp |])))

instance Eq v => Cnv ((v , Exp v) , Env v FAV.Exp) (FAV.Exp -> FAV.Exp) where
  cnv ((x , e) , r) = pure (frmRgtZro . curry cnv e . (: r) . (,) x)

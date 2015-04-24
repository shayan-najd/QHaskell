{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.Evaluation.ADTUntypedDebruijn
       () where

import QHaskell.MyPrelude

import QHaskell.Expression.ADTUntypedDebruijn
import qualified QHaskell.Expression.ADTValue as FAV

import QHaskell.Environment.Plain

import QHaskell.Conversion
import QHaskell.Variable.Conversion ()

instance Cnv (Exp , Env FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of
    Var v        -> pure (get v r)
    Non          -> impossibleM
    Som _        -> impossibleM
    May _ _ _    -> impossibleM
    Let el eb    -> FAV.leT  <$@> el <*@> eb
    _ -> $(biGenOverloadedML 'ee ''Exp "FAV"
     ['Var,'Non,'Som,'May,'Let]
     (const [| cnvImp |])))

instance Cnv (Fun , Env FAV.Exp)  (FAV.Exp -> FAV.Exp) where
  cnv (Fun e , r) = pure (frmRgtZro . curry cnv e . (: r))

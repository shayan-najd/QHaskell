module QHaskell.Expression.Conversions.TypeInference () where

import QHaskell.MyPrelude

import qualified QHaskell.Expression.GADTTyped           as FGTD

import qualified QHaskell.Type.Herbrand                           as TH
import qualified QHaskell.Type.ADT                       as TFA

import QHaskell.Environment.Scoped

import QHaskell.Conversion
import QHaskell.Variable.Conversion   ()

import QHaskell.Inference             (typInf)

instance n ~ n' =>
      Cnv (FGTD.Exp n (Maybe TFA.Typ) , Env n TFA.Typ)(FGTD.Exp n' TFA.Typ) where
  cnv (e , r) = let ?r = r in
    do r' :: Env n (TH.Typ (TH.EnvFld '[])) <- cnvImp r
       e'  <- traverse (maybe (return Nothing) (fmap Just . cnvImp))  e
       e'' <- typInf e' r'
       traverse cnvImp e''

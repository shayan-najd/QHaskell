module QHaskell.Expression.Conversions.TypeInference () where

import QHaskell.MyPrelude

import qualified QHaskell.Expression.GADTTyped           as GTD

import qualified QHaskell.Type.Herbrand                           as TH
import qualified QHaskell.Type.ADT                       as TA

import QHaskell.Environment.Scoped

import QHaskell.Conversion
import QHaskell.Variable.Conversion   ()

import QHaskell.Inference             (typInf)

instance (m ~ m' , n ~ n') =>
      Cnv (GTD.Exp m n (Maybe TA.Typ) , (Env m TA.Typ , Env n TA.Typ)) (GTD.Exp m' n' TA.Typ) where
  cnv (e , (s , g)) = do
    s' :: Env m (TH.Typ (TH.EnvFld '[])) <- cnv (s , ())
    g' :: Env n (TH.Typ (TH.EnvFld '[])) <- cnv (g, ())
    e'  <- traverse (maybe (return Nothing) (fmap Just . cnvWth ()))  e
    e'' <- typInf e' (s' , g')
    traverse (cnvWth ())  e''

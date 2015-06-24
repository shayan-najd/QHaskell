{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.ScopeWithnessing () where

import QHaskell.MyPrelude

import qualified QHaskell.Expression.ADTUntypedDebruijn as AUD
import qualified QHaskell.Expression.GADTTyped as GTD
import qualified QHaskell.Type.ADT as TA
import qualified QHaskell.Nat.ADT as NA

import QHaskell.Nat.GADT

import QHaskell.Conversion
import QHaskell.Variable.Conversion ()

instance (m ~ m' , n ~ n') =>
         Cnv (AUD.Exp , (Nat m , Nat n))
             (GTD.Exp m' n' (Maybe TA.Typ)) where
  cnv (ee  , r@(m , n)) = case ee of
    AUD.Var x         -> GTD.Var  <$> cnv (x , n)
    AUD.Prm x es      -> GTD.Prm  <$> pure (fmap (const Nothing) es) <*> cnv (x , m)
                                    <*> mapM (cnvWth r) es
    AUD.App ef ea     -> GTD.App  <$> pure Nothing  <*> cnvWth r ef <*> cnvWth r ea
    AUD.Fst e         -> GTD.Fst  <$> pure Nothing  <*> cnvWth r e
    AUD.Snd e         -> GTD.Snd  <$> pure Nothing  <*> cnvWth r e
    AUD.LeT el eb     -> GTD.LeT  <$> pure Nothing  <*> cnvWth r el <*> cnvWth r eb
    AUD.Typ t  e      -> GTD.Typ  <$> pure (Just t) <*> cnvWth r e
    _                  -> $(biGenOverloadedM 'ee ''AUD.Exp "GTD"
     ['AUD.App,'AUD.Fst,'AUD.Snd,'AUD.Prm,'AUD.Var,
      'AUD.LeT,'AUD.Typ] (const [| cnvWth r |]))

instance (m ~ m' , n ~ n') =>
         Cnv (AUD.Fun , (Nat m , Nat n)) (GTD.Exp m' (NA.Suc n') (Maybe TA.Typ)) where
  cnv (AUD.Fun e , (m , n)) = cnv (e , (m , Suc n))

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.ScopeWithnessing () where

import QHaskell.MyPrelude

import qualified QHaskell.Expression.ADTUntypedDebruijn  as FAUD
import qualified QHaskell.Expression.GADTTyped           as FGTD
import qualified QHaskell.Type.ADT                       as TFA
import qualified QHaskell.Nat.ADT                                 as NA
import QHaskell.Nat.GADT

import QHaskell.Conversion
import QHaskell.Variable.Conversion ()

instance n ~ n' => Cnv (FAUD.Exp , Nat n) (FGTD.Exp n' (Maybe TFA.Typ)) where
  cnv (ee  , n) = let ?r = n in case ee of
    FAUD.App ef ea    -> FGTD.App  <$> pure Nothing  <*@> ef <*@> ea
    FAUD.Fst e        -> FGTD.Fst  <$> pure Nothing  <*@> e
    FAUD.Snd e        -> FGTD.Snd  <$> pure Nothing  <*@> e
    FAUD.Let el eb    -> FGTD.Let  <$> pure Nothing  <*@> el <*@> eb
    FAUD.May em en es -> FGTD.May  <$> pure Nothing  <*@> em <*@> en <*@> es
    FAUD.Typ t  e     -> FGTD.Typ  <$> pure (Just t) <*@> e
    _                 -> $(biGenOverloadedM 'ee ''FAUD.Exp "FGTD"
     ['FAUD.App,'FAUD.Fst,'FAUD.Snd,
      'FAUD.Let,'FAUD.May,'FAUD.Typ] (const [| cnvImp |]))

instance n ~ n' => Cnv (FAUD.Fun , Nat n) (FGTD.Exp (NA.Suc n') (Maybe TFA.Typ)) where
  cnv (FAUD.Fun e , n) = cnv (e , Suc n)

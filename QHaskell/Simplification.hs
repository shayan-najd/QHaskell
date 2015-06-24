{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Simplification (smp) where

import QHaskell.MyPrelude

import QHaskell.Expression.GADTFirstOrder
import QHaskell.Expression.Utils.Equality.GADTFirstOrder
import QHaskell.Expression.Utils.GADTFirstOrder
    (sbs,cntVar)
import QHaskell.Variable.Typed
import QHaskell.Singleton
import QHaskell.ChangeMonad
import QHaskell.Expression.Utils.Common
import qualified QHaskell.Type.GADT as TG

smp :: TG.Type a => Exp s g a -> Exp s g a
smp = tilNotChg smpOne

smpOne :: forall s g a. TG.Type a =>
          Exp s g a -> Chg (Exp s g a)
smpOne ee = let t = sin :: TG.Typ a in case ee of
    Prm x ns -> Prm x <$> TG.mapMC smpOne ns
    LeT m n
      | cntVar Zro n <= 1 -> chg (sbs m n)
    Cnd _ m n
      | eql m n     -> chg m
    _       -> $(genOverloadedMW 'ee ''Exp  ['Prm] (trvWrp 't)
      (\ tt -> if
           | matchQ tt [t| Exp a a a |] -> [| smpOne |]
           | otherwise                  -> [| pure   |]))

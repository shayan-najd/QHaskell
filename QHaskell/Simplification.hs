module QHaskell.Simplification (smp) where

import QHaskell.MyPrelude

import QHaskell.Expression.GADTFirstOrder
import QHaskell.Expression.Utils.GADTFirstOrder (sbs,cntVar)
import QHaskell.Variable.Typed
import QHaskell.Singleton
import QHaskell.ChangeMonad
import QHaskell.Expression.Utils.Common
import qualified QHaskell.Type.GADT as TFG

smp :: HasSin TFG.Typ a => Exp g a -> Exp g a
smp = tilNotChg smpOne

smpOne :: forall g a. HasSin TFG.Typ a =>
          Exp g a -> Chg (Exp g a)
smpOne ee = let t = sin :: TFG.Typ a in case ee of
    Let m n
      | cntVar Zro n <= 1 -> chg (sbs m n)
    _       -> $(genOverloadedMW 'ee ''Exp  [] (trvWrp 't)
      (\ tt -> if
           | matchQ tt [t| Exp a a |]            -> [| smpOne |]
           | otherwise                           -> [| pure   |]))

module QHaskell.Normalisation (nrm) where

import QHaskell.MyPrelude

import QHaskell.Expression.GADTFirstOrder
import QHaskell.Expression.Utils.GADTFirstOrder
    (sucAll,sbs,replaceOne,cntVar,pattern TF,pattern V,pattern NV)
import QHaskell.Variable.Typed
import QHaskell.Singleton
import QHaskell.ChangeMonad
import QHaskell.Expression.Utils.Common
import qualified QHaskell.Type.GADT as TFG

nrm :: HasSin TFG.Typ a => Exp g a -> Exp g a
nrm = tilNotChg nrmOne

nrmOne :: forall g a. HasSin TFG.Typ a => Exp g a -> Chg (Exp g a)
nrmOne ee = let t = sin :: TFG.Typ a in case ee of
    App ef                      (NV ea) -> chg (Let ea (App (sucAll ef) (Var Zro)))
    App (TF (Abs eb))           (V  ea) -> chg (sbs ea eb)
    App (TF (Let (NV el) eb))   (V  ea) -> chg (Let el (App eb (sucAll ea)))

    Tpl (NV ef) es               -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> chg (Let ef (Tpl (Var Zro) (sucAll es)))
    Tpl (V ef)  (NV es)          -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> chg (Let es (Tpl (sucAll ef) (Var Zro)))

    Fst (NV e)                   -> chg (Let e (Fst (Var Zro)))
    Fst (TF (Tpl (V ef) (V _)))  -> chg  ef

    Snd (NV e)                   -> chg (Let e (Snd (Var Zro)))
    Snd (TF (Tpl (V _)  (V es))) -> chg  es

    Let (TF (Let (NV el') eb'))  eb   -> chg (Let el' (Let eb' (replaceOne eb)))
    Let (V v)               eb   -> chg (sbs v eb)
    Let (NV v)         eb
      | cntVar Zro eb == 0       -> chg (sbs v eb)

    Som (NV e)                   -> case TFG.getPrfHasSinMay t of
      PrfHasSin                  -> chg (Let e  (Som (Var Zro)))

    May (NV em) en      es       -> chg (Let em (May (Var Zro)   (sucAll en) (sucAll es)))
    May (V  em) (NV en) es       -> chg (Let en (May (sucAll em) (Var Zro)   (sucAll es)))
    May (V  em) (V  en) (NV es)  -> chg (Let es (May (sucAll em) (sucAll en) (Var Zro)))
    May (TF Non) en      _       -> chg en
    May (TF (Som e)) _       es  -> chg (App es e)

    Int i                        -> case t of
      TFG.Int                    -> chg (ConI i)
      TFG.Flt                    -> chg (ConF (fromIntegral i))
      _                          -> fail "Type Error3!"

    Mem (NV e)                   -> chg (Let e (Mem (Var Zro)))

    _                            -> $(genOverloadedMW 'ee ''Exp  [] (trvWrp 't)
     (\ tt -> if
      | matchQ tt [t| Exp a a |] -> [| nrmOne |]
      | otherwise                -> [| pure   |]))

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Normalisation (nrm) where

import QHaskell.MyPrelude hiding (fmap,foldl)

import QHaskell.Expression.GADTFirstOrder
import QHaskell.Expression.Utils.GADTFirstOrder
    (sucAll,sbs,replaceOne,cntVar,pattern TF,pattern V,pattern NV,isVal)
import QHaskell.Variable.Typed
import QHaskell.Singleton
import QHaskell.ChangeMonad
import QHaskell.Expression.Utils.Common
import qualified QHaskell.Type.GADT as TG
import QHaskell.Environment.Typed
import QHaskell.Magic

nrm :: TG.Type a => Bool -> Exp s g a -> Exp s g a
nrm b = tilNotChg (nrmOne b)

cmt :: forall a s g d d' as c.
       (Match c as a , TG.Type a , as ~ Add d d' , TG.Types d , TG.Types d') =>
       Var s c -> Env (Exp s g) d -> Env (Exp s g) d' -> Chg (Exp s g a)
cmt x d d' = do
  let tsd  = sin :: Env TG.Typ d
  let tsd' = sin :: Env TG.Typ d'
  PrfHasSin <- getPrfHasSinM (add tsd tsd')
  case d' of
    Emp           -> return (Prm x (add d d'))
    Ext (NV e) es -> case TG.getPrfHasSinEnvOf d' of
     (PrfHasSin,PrfHasSin) -> chg (LeT e (Prm x (add (fmap sucAll d) (Ext (Var Zro) (fmap sucAll es)))))
    Ext (e :: Exp s g te) (es :: Env (Exp s g) tes) -> case TG.getPrfHasSinEnvOf d' of
     (PrfHasSin,PrfHasSin) -> case obvious :: Add (Add d (te ': '[])) tes :~: Add d (te ': tes) of
         Rfl -> do PrfHasSin <- getPrfHasSinM (add tsd (Ext (sinTyp e) Emp))
                   cmt x (add d (Ext e Emp)) es

hasNV :: Env (Exp s g) d -> Bool
hasNV = foldl (\ b e -> b || (not (isVal e))) False

nrmOne :: forall s g a. TG.Type a => Bool -> Exp s g a -> Chg (Exp s g a)
nrmOne b ee = let t = sin :: TG.Typ a in case ee of
    Prm x es
      | b && hasNV es -> cmt x Emp es
      | otherwise     -> Prm x <$> TG.mapMC (nrmOne b) es

    App ef                      (NV ea) -> chg (LeT ea (App (sucAll ef) (Var Zro)))
    App (TF (Abs eb))           (V  ea) -> chg (sbs ea eb)
    App (TF (Cnd (V ec) et ef)) (V  ea) -> chg (Cnd ec (App et ea) (App ef ea))
    App (TF (LeT (NV el) eb))   (V  ea) -> chg (LeT el (App eb (sucAll ea)))

    Cnd (NV ec)           et ef -> chg (LeT ec (Cnd (Var Zro) (sucAll et) (sucAll ef)))
    Cnd (TF (ConB True))  et _  -> chg et
    Cnd (TF (ConB False)) _  ef -> chg ef

    Tpl (NV ef) es | b           -> case TG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> chg (LeT ef (Tpl (Var Zro) (sucAll es)))
    Tpl (V ef)  (NV es) | b      -> case TG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> chg (LeT es (Tpl (sucAll ef) (Var Zro)))

    Fst (NV e)  | b              -> chg (LeT e (Fst (Var Zro)))
    Fst (TF (Tpl (V ef) (V _)))  -> chg  ef

    Snd (NV e)  | b              -> chg (LeT e (Snd (Var Zro)))
    Snd (TF (Tpl (V _)  (V es))) -> chg  es

    LeT (TF (LeT (NV el') eb'))  eb | b  -> chg (LeT el' (LeT eb' (replaceOne eb)))
    LeT (TF (Cnd ec et ef))      eb   -> chg (Cnd ec (LeT et eb) (LeT ef eb))
    LeT (V v)               eb   -> chg (sbs v eb)
    LeT (NV v)         eb
      | cntVar Zro eb == 0       -> chg (sbs v eb)

    Int i                        -> case t of
      TG.Wrd                    -> chg (ConI i)
      TG.Flt                    -> chg (ConF (fromIntegral i))
      _                          -> fail "Type Error3!"

    Mem (NV e)                   -> chg (LeT e (Mem (Var Zro)))

    _                            -> $(genOverloadedMW 'ee ''Exp  ['Prm] (trvWrp 't)
     (\ tt -> if
      | matchQ tt [t| Exp a a a |] -> [| nrmOne b |]
      | otherwise                  -> [| pure     |]))

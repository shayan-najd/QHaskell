{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.Evaluation.GADTHigherOrder () where

import QHaskell.MyPrelude
import QHaskell.Expression.GADTHigherOrder
import qualified QHaskell.Expression.GADTValue as FGV
import qualified QHaskell.Type.GADT as TG
import QHaskell.Environment.Typed hiding (fmap)
import QHaskell.Conversion
import QHaskell.Variable.Conversion ()
import QHaskell.Singleton
import QHaskell.Expression.Utils.Common

instance (TG.Type a , a ~ a') =>
         Cnv (Exp s a , Env FGV.Exp s) (FGV.Exp a') where
  cnv (ee , s) = let t = sin :: TG.Typ a in case ee of
    Tmp _    -> impossibleM
    Prm x es -> FGV.prm  (get x s) <$>
                TG.mapMC (cnvWth s)  es
    _  -> $(biGenOverloadedMWL 'ee ''Exp "FGV"
            ['Tmp,'Prm] (trvWrp 't)
            (\ tt -> if
                 | matchQ tt [t| Exp a a -> Exp a a |] ->
                     [| \ f -> pure
                               (FGV.Exp
                                (FGV.getTrm
                                 . frmRgtZro
                                 . cnvWth s
                                 . f
                                 . frmRgtZro
                                 . cnvWth s
                                 . FGV.Exp )) |]
                 | matchQ tt [t| Exp a a |] ->
                     [| \ e -> cnv (e , s) |]
                 | otherwise                -> [| pure |]))

instance (TG.Type t , r ~ r' , t ~ t') =>
         Cnv (FGV.Exp t' , Env FGV.Exp r') (Exp r t)
         where
  cnv (FGV.Exp v , r) = let t = sin :: TG.Typ t in case t of
    TG.Wrd                   -> pure (ConI v)
    TG.Bol                   -> pure (ConB v)
    TG.Flt                   -> pure (ConF v)
    TG.Arr (_ :: TG.Typ b) (_ :: TG.Typ c) -> case TG.getPrfHasSinArr t of
     (PrfHasSin , PrfHasSin)  -> Abs  <$> pure (frmRgtZro
                                                . cnvWth r
                                                . (fmap v :: FGV.Exp b -> FGV.Exp c)
                                                . frmRgtZro
                                                . cnvWth r)
    TG.Tpl _ _               -> case TG.getPrfHasSinTpl t of
     (PrfHasSin , PrfHasSin)  -> Tpl  <$> cnv (FGV.Exp (fst v) , r)
                                      <*> cnv (FGV.Exp (snd v) , r)

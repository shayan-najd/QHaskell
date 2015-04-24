{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.Evaluation.GADTHigherOrder () where

import QHaskell.MyPrelude
import QHaskell.Expression.GADTHigherOrder
import qualified QHaskell.Expression.GADTValue as FGV
import qualified QHaskell.Type.GADT as TFG
import QHaskell.Environment.Typed hiding (fmap)
import QHaskell.Conversion
import QHaskell.Variable.Conversion ()
import QHaskell.Singleton
import QHaskell.Expression.Utils.Common

instance (HasSin TFG.Typ t , t' ~ t) =>
         Cnv (Exp r t , Env FGV.Exp r) (FGV.Exp t') where
  cnv (ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case ee of
    Var v                    -> pure (get v r)
    Non                      -> impossibleM
    Som _                    -> impossibleM
    May _ _ _                -> impossibleM
    Tmp _                    -> impossibleM
    Let el eb                -> FGV.leT <$@> el <*@> eb
    Int i                    -> case t of
      TFG.Int                -> pure (FGV.conI i)
      TFG.Flt                -> pure (FGV.conF (fromIntegral i))
      _                      -> fail "Type Error in Int"
    Tag s e                  -> FGV.tag s <$@> e
    _  -> $(biGenOverloadedMWL 'ee ''Exp "FGV"
            ['Var,'Non,'Som,'May,'Let,'Tmp,'Int,'Tag]
            (trvWrp 't) (const [| cnvImp |]))

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb
         , ta' ~ ta , tb' ~ tb) =>
         Cnv (Exp r ta -> Exp r tb , Env FGV.Exp r)
             (FGV.Exp (Arr ta' tb')) where
  cnv (f , r)  =  let ?r = r in
    pure (FGV.Exp ( FGV.getTrm
                  . frmRgtZro . cnvImp
                  . f
                  . frmRgtZro . cnvImp
                  . FGV.Exp ))

instance (HasSin TFG.Typ t , r ~ r' , t ~ t') =>
         Cnv (FGV.Exp t' , Env FGV.Exp r') (Exp r t)
         where
  cnv (FGV.Exp v , r) = let ?r = r in let t = sin :: TFG.Typ t in case t of
    TFG.Int                   -> pure (ConI v)
    TFG.Bol                   -> pure (ConB v)
    TFG.Flt                   -> pure (ConF v)
    TFG.Arr _ _               -> case TFG.getPrfHasSinArr t of
     (PrfHasSin , PrfHasSin)  -> Abs  <$@> samTyp t (FGV.Exp v)
    TFG.Tpl _ _               -> case TFG.getPrfHasSinTpl t of
     (PrfHasSin , PrfHasSin)  -> Tpl  <$@> FGV.Exp (fst v)
                                      <*@> FGV.Exp (snd v)
    TFG.May _                 -> impossibleM


instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , r ~ r' , ta ~ ta' , tb ~ tb')=>
         Cnv (FGV.Exp (Arr ta' tb') , Env FGV.Exp r') (Exp r ta -> Exp r tb)
         where
  cnv (FGV.Exp f , r) = let ?r = r in
    pure (frmRgtZro . cnvImp . (fmap f :: FGV.Exp ta -> FGV.Exp tb) . frmRgtZro . cnvImp)

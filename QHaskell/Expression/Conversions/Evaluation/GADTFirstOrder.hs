{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.Evaluation.GADTFirstOrder () where

import QHaskell.MyPrelude
import QHaskell.Expression.GADTFirstOrder
import qualified QHaskell.Expression.GADTValue as FGV
import qualified QHaskell.Type.GADT as TFG
import QHaskell.Environment.Typed
import QHaskell.Conversion
import QHaskell.Variable.Conversion ()
import QHaskell.Singleton
import QHaskell.Expression.Utils.Common

instance (HasSin TFG.Typ t , t' ~ t) =>
         Cnv (Exp r t , Env FGV.Exp r) (FGV.Exp t') where
  cnv (ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case ee of
    Var  v                   -> pure (get v r)
    Non                      -> impossibleM
    Som _                    -> impossibleM
    May _ _ _                -> impossibleM
    Int i                    -> case t of
      TFG.Int                -> pure (FGV.conI i)
      TFG.Flt                -> pure (FGV.conF (fromIntegral i))
      _                      -> fail "Type Error in Int"
    Let el eb                -> FGV.leT <$@> el <*@> eb
    Tag s e                  -> FGV.tag s <$@> e
    _  -> $(biGenOverloadedMWL 'ee ''Exp "FGV"
     ['Var,'Non,'Som,'May,'Let,'Int,'Tag]
     (trvWrp 't) (const [| cnvImp |]))

instance (ta' ~ ta , tb' ~ tb , HasSin TFG.Typ ta , HasSin TFG.Typ tb) =>
         Cnv (Exp (ta ': r) tb , Env FGV.Exp r)  (FGV.Exp (Arr ta' tb'))
         where
  cnv  (e , r) = (pure . FGV.Exp)
                  (FGV.getTrm . frmRgtZro . curry cnv e
                   . flip Ext r . (FGV.Exp :: ta -> FGV.Exp ta))

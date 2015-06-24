{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.Evaluation.GADTFirstOrder () where

import QHaskell.MyPrelude
import QHaskell.Expression.GADTFirstOrder
import qualified QHaskell.Expression.GADTValue as FGV
import qualified QHaskell.Type.GADT as TG
import QHaskell.Environment.Typed
import QHaskell.Conversion
import QHaskell.Variable.Conversion ()
import QHaskell.Singleton
import QHaskell.Expression.Utils.Common

instance (TG.Type a', a ~ a') =>
         Cnv (Exp s g a , (Env FGV.Exp s, Env FGV.Exp g)) (FGV.Exp a')
  where
  cnv (ee , r@(s , g)) = let t = sin :: TG.Typ a in case ee of
    Var x    -> pure (get x g)
    Prm x es -> FGV.prm (get x s) <$> TG.mapMC (cnvWth r) es
    _        -> $(biGenOverloadedMWL 'ee ''Exp "FGV" ['Var,'Prm]
                  (trvWrp 't)
     (\ tt -> if
       | matchQ tt [t| Exp a (a ': a) a |] ->
           [| \ e -> (pure . FGV.Exp)
                (\ v -> FGV.getTrm
                  (frmRgtZro (cnv (e , (s , Ext (FGV.Exp v) g)))))|]
       | matchQ tt [t| Exp a a a |] -> [| cnvWth r |]
       | otherwise                  -> [| pure |]))

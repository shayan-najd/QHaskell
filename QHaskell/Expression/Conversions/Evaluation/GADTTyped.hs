{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.Evaluation.GADTTyped () where

import QHaskell.MyPrelude
import QHaskell.Expression.GADTTyped
import qualified QHaskell.Expression.ADTValue as FAV
import QHaskell.Environment.Scoped
import QHaskell.Nat.ADT
import QHaskell.Conversion
import QHaskell.Variable.Conversion ()

instance Cnv (Exp m n t , (Env m FAV.Exp , Env n FAV.Exp)) FAV.Exp where
  cnv (ee , r@(s , g)) = join (case ee of
    Prm _ x es   -> FAV.prm (get x s) <$> mapM (\ e -> cnv (e , r))  es
    Var x        -> pure (pure (get x g))
    App _  ef ea -> FAV.app  <$> cnv (ef , r) <*> cnv (ea , r)
    Fst _  e     -> FAV.fst  <$> cnv (e , r)
    Snd _  e     -> FAV.snd  <$> cnv (e , r)
    LeT _  el eb -> FAV.leT  <$> cnv (el , r) <*> cnv (eb , r)
    Typ _ e      -> pure (cnv (e , r))
    _ -> $(biGenOverloadedML 'ee ''Exp "FAV"
     ['Prm,'Var,'App,'Fst,'Snd,'LeT,'Typ]
      (const [| \ e -> cnv (e , r) |])))

instance Cnv (Exp m (Suc n) a , (Env m FAV.Exp , Env n FAV.Exp)) (FAV.Exp -> FAV.Exp) where
  cnv (e , (s , g)) = pure (\ x -> frmRgtZro (cnv (e , (s , Ext x g))))

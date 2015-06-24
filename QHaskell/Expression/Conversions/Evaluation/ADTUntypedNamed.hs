{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.Evaluation.ADTUntypedNamed () where

import QHaskell.MyPrelude

import QHaskell.Expression.ADTUntypedNamed
import qualified QHaskell.Expression.ADTValue as FAV
import QHaskell.Environment.Map
import QHaskell.Conversion

instance (Show x , Eq x) => Cnv (Exp x , (Env x FAV.Exp , Env x FAV.Exp)) FAV.Exp where
  cnv (ee , r@(s , g)) = join (case ee of
    Var x    -> FAV.var <$> get x g
    Prm x es -> FAV.prm <$> get x s <*> mapM (\ e -> cnv (e,r)) es
    _        -> $(biGenOverloadedML 'ee ''Exp "FAV" ['Prm,'Var]
     (\ tt -> if
       | matchQ tt [t| Exp x |]       ->
           [| \ e -> cnv (e , r) |]
       | matchQ tt [t| (x , Exp x) |] ->
           [| \ (x , e) ->
                pure (\ v -> frmRgtZro (cnv (e  , (s , (x,v) : g)))) |]
       | otherwise                    ->
           [| pure |])))

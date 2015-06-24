{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.Evaluation.ADTUntypedDebruijn
       () where

import QHaskell.MyPrelude

import QHaskell.Expression.ADTUntypedDebruijn
import qualified QHaskell.Expression.ADTValue as FAV
import QHaskell.Environment.Plain
import QHaskell.Conversion

instance Cnv (Exp , (Env FAV.Exp , Env FAV.Exp)) FAV.Exp where
  cnv (ee , r@(s , g)) = join (case ee of
    Var x        -> FAV.var  <$> get x g
    Prm x es     -> FAV.prm  <$> get x s <*> mapM (\ e -> cnv (e , r)) es
    _ -> $(biGenOverloadedML 'ee ''Exp "FAV"
     ['Prm,'Var]
     (\ tt -> if
       | matchQ tt [t| Exp |] -> [| \ e -> cnv (e , r) |]
       | matchQ tt [t| Fun |] -> [| \ (Fun e) -> pure
                                        (\ v -> frmRgtZro (cnv (e  , (s , v : g)))) |]
       | otherwise            -> [| pure   |])))

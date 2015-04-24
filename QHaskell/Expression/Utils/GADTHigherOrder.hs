{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Utils.GADTHigherOrder where

import QHaskell.MyPrelude
import QHaskell.Expression.Utils.Common
import QHaskell.Expression.GADTHigherOrder
import QHaskell.Variable.Typed
import qualified QHaskell.Type.GADT as TFG
import QHaskell.Singleton

sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc prd

prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar prd Suc

mapVar :: forall t r r'.
          (forall t'. Var r  t' -> Var r' t') ->
          (forall t'. Var r' t' -> Var r  t') ->
          Exp r t -> Exp r' t
mapVar f g ee = case ee of
  Var v -> Var (f v)
  _     -> $(genOverloaded 'ee ''Exp ['Var]
   (\ t -> if
    | matchQ t [t| Exp t t -> Exp t t |] ->
        [| \ ff -> mapVar f g . ff . mapVar g f |]
    | matchQ t [t| Exp t t |]          ->
        [| mapVar f g |]
    | otherwise                        ->
        [| id |]))

absTmp :: forall r t t'. (HasSin TFG.Typ t', HasSin TFG.Typ t) =>
          Exp r t' -> String -> Exp r t -> Exp r t
absTmp xx s ee = let t = sin :: TFG.Typ t in case ee of
  Tmp x
    | s == x    -> case eqlSin (sinTyp xx) (sin :: TFG.Typ t) of
      Rgt Rfl   -> xx
      _         -> ee
    | otherwise -> ee
  _             -> $(genOverloadedW 'ee ''Exp  ['Tmp] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| (absTmp xx s .) |]
    | matchQ tt [t| Exp t t |]            -> [| absTmp xx s |]
    | otherwise                           -> [| id |]))

module QHaskell.Expression.Conversions.TypeWithnessing () where

import QHaskell.MyPrelude
import qualified QHaskell.Expression.GADTTyped      as FGTD
import qualified QHaskell.Expression.GADTFirstOrder as FGFO

import qualified QHaskell.Type.GADT                 as TFG
import qualified QHaskell.Type.ADT                  as TFA

import QHaskell.Environment.Typed

import QHaskell.Conversion
import QHaskell.Variable.Conversion      ()

import QHaskell.Singleton

type ExsTyp = ExsSin TFG.Typ

instance (r ~ r' , n ~ Len r , HasSin TFG.Typ t) =>
         Cnv (FGTD.Exp n TFA.Typ , Env TFG.Typ r)
             (FGFO.Exp r' t) where
  cnv (ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case ee of
    FGTD.ConI i       -> case t of
      TFG.Int         -> pure (FGFO.ConI i)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.ConB b       -> case t of
      TFG.Bol         -> pure (FGFO.ConB b)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.ConF f       -> case t of
      TFG.Flt         -> pure (FGFO.ConF f)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Abs eb       -> case t of
      TFG.Arr ta _    -> case TFG.getPrfHasSinArr t of
       (PrfHasSin , PrfHasSin) -> FGFO.Abs <$@> (ta , eb)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Tpl ef es    -> case t of
      TFG.Tpl _ _     -> case TFG.getPrfHasSinTpl t of
       (PrfHasSin , PrfHasSin) -> FGFO.Tpl <$@> ef <*@> es
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Non          -> case t of
     TFG.May _        -> case TFG.getPrfHasSinMay t of
      PrfHasSin       -> pure FGFO.Non
     _                -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Som e        -> case t of
     TFG.May _        -> case TFG.getPrfHasSinMay t of
      PrfHasSin       -> FGFO.Som <$@> e
     _                -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Var x        -> FGFO.Var  <$@> x
    FGTD.App ta ef ea -> do ExsSin ta' :: ExsTyp <- cnv ta
                            PrfHasSin <- getPrfHasSinM ta'
                            ea' <- cnvImp ea
                            FGFO.App <$@> ef
                                    <*> pure (samTyp ta' ea')
    FGTD.Fst ts e     -> do ExsSin ts' <- cnv ts
                            PrfHasSin  <- getPrfHasSinM ts'
                            e'         <- cnvImp e
                            FGFO.Fst <$> pure
                                    (samTyp (TFG.Tpl t ts') e')
    FGTD.Snd tf e     -> do ExsSin tf' <- cnv tf
                            PrfHasSin  <- getPrfHasSinM tf'
                            e'         <- cnvImp e
                            FGFO.Snd <$> pure
                                    (samTyp (TFG.Tpl tf' t) e')
    FGTD.Let tl el eb -> do ExsSin tl' :: ExsTyp <- cnv tl
                            PrfHasSin <- getPrfHasSinM tl'
                            FGFO.Let <$@> el <*@> (tl' , eb)
    FGTD.May tm em en es -> do ExsSin t' :: ExsTyp <- cnv tm
                               PrfHasSin <- getPrfHasSinM t'
                               em' <- cnvImp em
                               FGFO.May
                                       <$> pure (samTyp (TFG.May t') em')
                                       <*@> en <*@> es
    FGTD.Typ _ e      -> cnvImp e
    FGTD.Int i        -> case t of
      TFG.Int         -> pure (FGFO.Int i)
      TFG.Flt         -> pure (FGFO.Int i)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Mem e        -> FGFO.Mem <$@> e

instance (r ~ r' , n ~ Len (tr ': r) , HasSin TFG.Typ t , tr ~ tr') =>
         Cnv ((TFG.Typ tr , FGTD.Exp n TFA.Typ) , Env TFG.Typ r)
             (FGFO.Exp (tr' ': r') t) where
  cnv ((t , ee) , r) = cnv (ee , Ext t r)

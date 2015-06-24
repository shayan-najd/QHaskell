module QHaskell.Expression.Conversions.TypeWithnessing () where

import QHaskell.MyPrelude hiding (lookup)
import qualified QHaskell.Expression.GADTTyped as GTD
import qualified QHaskell.Expression.GADTFirstOrder as GFO
import qualified QHaskell.Type.GADT as TG
import qualified QHaskell.Type.ADT as TA
import QHaskell.Environment.Typed
import QHaskell.Conversion
import QHaskell.Variable.Conversion ()
import QHaskell.Type.Conversion ()
import QHaskell.Singleton
import qualified QHaskell.Variable.Typed as VT
import QHaskell.Magic

type ExsTyp = ExsSin TG.Typ

cnvEnv :: forall s g d. (TG.Types d) =>
          Env TG.Typ s -> Env TG.Typ g ->
          [GTD.Exp (Len s) (Len g) TA.Typ] -> NamM ErrM (Env (GFO.Exp s g) d)
cnvEnv s g ess = let d = sin :: Env TG.Typ d in case ess of
  []        -> case d of
   Emp      -> return Emp
   Ext _ _  -> fail "Type Error!"
  e : es    -> case d of
   Emp      -> fail "Type Error!"
   Ext _ _  -> case TG.getPrfHasSinEnvOf d of
    (PrfHasSin , PrfHasSin) -> Ext <$> cnvWth (s , g) e <*> cnvEnv s g es

instance (s ~ s' , g ~ g' , m ~ (Len s) , n ~ (Len g) , TG.Type a) =>
         Cnv (GTD.Exp m n TA.Typ , (Env TG.Typ s , Env TG.Typ g))
             (GFO.Exp s' g' a) where
  cnv (ee , r@(s , g)) = let t = sin :: TG.Typ a in case ee of
    GTD.ConI i       -> case t of
      TG.Wrd         -> pure (GFO.ConI i)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.ConB b       -> case t of
      TG.Bol         -> pure (GFO.ConB b)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.ConF f       -> case t of
      TG.Flt         -> pure (GFO.ConF f)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Prm ts x es  -> do ExsSin (as :: Env TG.Typ as):: ExsSin (Env TG.Typ) <- cnv (ts , ())
                           PrfHasSin <- getPrfHasSinM as
                           es' :: Env (GFO.Exp s g) as <- cnvEnv s g es
                           Exs1 (tx :: TG.Typ xa)
                             (x' :: VT.Var s xa) <- cnv (x , s)
                           PrfMatch <- getPrfMatch tx as t
                           return (GFO.Prm x' es')
    GTD.Abs eb       -> case t of
      TG.Arr ta _    -> case TG.getPrfHasSinArr t of
       (PrfHasSin , PrfHasSin) -> GFO.Abs <$> cnvWth r (ta , eb)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Tpl ef es    -> case t of
      TG.Tpl _ _     -> case TG.getPrfHasSinTpl t of
       (PrfHasSin , PrfHasSin) -> GFO.Tpl <$> cnvWth r ef <*> cnvWth r es
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Var x        -> GFO.Var  <$> cnv (x , g)
    GTD.App ta ef ea -> do ExsSin ta' :: ExsTyp <- cnv ta
                           PrfHasSin <- getPrfHasSinM ta'
                           ea' <- cnvWth r ea
                           GFO.App <$> cnvWth r ef
                                  <*> pure (samTyp ta' ea')
    GTD.Cnd ec et ef -> GFO.Cnd <$> cnvWth r ec <*> cnvWth r et <*> cnvWth r ef
    GTD.Fst ts e     -> do ExsSin ts' <- cnv ts
                           PrfHasSin  <- getPrfHasSinM ts'
                           e'         <- cnvWth r e
                           GFO.Fst <$> pure
                                    (samTyp (TG.Tpl t ts') e')
    GTD.Snd tf e     -> do ExsSin tf' <- cnv tf
                           PrfHasSin  <- getPrfHasSinM tf'
                           e'         <- cnvWth r e
                           GFO.Snd <$> pure
                                    (samTyp (TG.Tpl tf' t) e')
    GTD.LeT tl el eb -> do ExsSin tl' :: ExsTyp <- cnv tl
                           PrfHasSin <- getPrfHasSinM tl'
                           GFO.LeT <$> cnvWth r el <*> cnvWth r (tl' , eb)
    GTD.Typ _ e      -> cnvWth r e
    GTD.Int i        -> case t of
      TG.Wrd         -> pure (GFO.Int i)
      TG.Flt         -> pure (GFO.Int i)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Mem e        -> GFO.Mem <$> cnvWth r e
    GTD.Fix e        -> GFO.Fix <$> cnvWth r e

instance (s ~ s' , g ~ g' , a ~ a' , n ~ (Len (a ': g)) , m ~ (Len s), TG.Type b) =>
         Cnv ((TG.Typ a , GTD.Exp m n TA.Typ) , (Env TG.Typ s , Env TG.Typ g ))
             (GFO.Exp s' (a' ': g') b) where
  cnv ((t , ee) , (s , g)) = cnv (ee , (s ,  Ext t g))

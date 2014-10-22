module QFeldspar.Simplify  where

import QFeldspar.MyPrelude hiding (foldl,fmap)

import QFeldspar.Expression.Feldspar.MiniWellScoped

import Data.IORef
import System.IO.Unsafe
import QFeldspar.Singleton
import qualified QFeldspar.Type.Feldspar.GADT as TFG
import QFeldspar.Environment.Typed
import QFeldspar.ChangeMonad

class SmpOne a where
  smpOne :: a -> Chg a

infixl 4 <$@>
(<$@>) :: SmpOne a => (a -> b) -> a -> Chg b
el <$@> er = el <$> smpOne er

infixl 4 <*@>
(<*@>) :: SmpOne a => Chg (a -> b) -> a -> Chg b
el <*@> er = el <*> smpOne er

instance (HasSin TFG.Typ t) =>
         SmpOne (Exp n t) where
  smpOne ee = let t = sin :: TFG.Typ t in case ee of
    ConI i                       -> pure (ConI i)
    ConB b                       -> pure (ConB b)
    ConF f                       -> pure (ConF f)
    AppV x             es        -> AppV x <$> TFG.mapMC (sinTypOf x t) smpOne es
    Cnd ec           et ef       -> Cnd  <$@> ec <*@> et <*@> ef
    Whl ec eb ei                 -> Whl  <$@> ec <*@> eb <*@> ei
    Tpl ef      es               -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> Tpl  <$@> ef <*@> es
    Fst e                        -> Fst  <$@> e
    Snd e                        -> Snd  <$@> e
    Ary el      ef               -> case TFG.getPrfHasSinAry t of
      PrfHasSin                  -> Ary  <$@> el <*@> ef
    Len e                        -> Len  <$@> e
    Ind ea             ei        -> Ind  <$@> ea <*@> ei
    Cmx er ei                    -> Cmx  <$@> er <*@> ei

    Let ea         eb
      | hasOneOrZro eb               -> chg (eb ea)
    Let el             eb        -> Let  <$@> el <*@> eb
    Tmp x                        -> pure (Tmp x)
    Tag x e                      -> Tag x <$@> e
    Non                          -> pure Non
    Som e                        -> case TFG.getPrfHasSinMay t of
      PrfHasSin                  -> Som  <$@> e
    May em      en      es       -> May  <$@> em <*@> en <*@> es


ref :: IORef Int
{-# NOINLINE ref #-}
ref = unsafePerformIO (newIORef 0)

instance (HasSin TFG.Typ tb, HasSin TFG.Typ ta) =>
         SmpOne (Exp n ta -> Exp n tb) where
  smpOne f = let i = unsafePerformIO (do j <- readIORef ref
                                         modifyIORef ref (+1)
                                         return j)
                 v = "_xn" ++ show i
             in do eb <- smpOne (f (Tmp v))
                   return (\ x -> absTmp x v eb)

smpEnv :: (TFG.Arg t ~ r') =>
               (TFG.Typ t , Env (Exp r) r') -> Chg (Env (Exp r) r')
smpEnv (TFG.Arr t ts , Ext e es) = case getPrfHasSin t of
    PrfHasSin -> do e'  <- smpOne e
                    es' <- smpEnv (ts , es)
                    pure (Ext e' es')
smpEnv (TFG.Arr _ _  , _)        = impossibleM
smpEnv (_            , Emp)      = pure Emp
smpEnv (_            , Ext _ _)  = impossibleM
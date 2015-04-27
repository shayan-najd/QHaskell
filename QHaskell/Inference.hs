module QHaskell.Inference where

import QHaskell.MyPrelude

import QHaskell.Nat.ADT
import qualified QHaskell.Type.Herbrand as TH
import QHaskell.Type.Herbrand hiding (Tpl,May,App,Int)
import QHaskell.Solver
import QHaskell.Conversion
import QHaskell.Nat.Conversion ()
import QHaskell.Expression.GADTTyped
import QHaskell.Environment.Scoped  as ES
import QHaskell.InferenceMonad

type TypFld = Typ (EnvFld '[])

mxm :: [Nat] -> Nat
mxm [] = Zro
mxm l  = maximum l

ind :: Exp n (Maybe TypFld) -> Exp n TypFld
ind e = (flip evalState
         (succ (maxMta (fmap (maybe (Mta Zro) id) e))) .
         mapM (maybe (do i <- getState
                         put (Suc i)
                         return (Mta i)) return)) e

typInf :: Exp n (Maybe TypFld) -> ES.Env n TypFld ->
          NamM ErrM (Exp n TypFld)
typInf e r = inf (ind e) r

maxMta :: Exp n TypFld -> Nat
maxMta = mxm . concat . fmap mtas . toList

inf :: Exp n TypFld -> ES.Env n TypFld -> NamM ErrM (Exp n TypFld)
inf e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (i' , cs) = execState (collect e r) (succ mts , [])
             mTs <- lift (slv (fmap Mta [Zro .. pred i']) cs)
             let ttas = zip [Zro ..] mTs

             return (fmap (appTtas ttas) e)

chk :: Exp n TypFld -> ES.Env n TypFld -> NamM ErrM TypFld
chk e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (t , (i' , cs)) = runState (collect e r) (mts , [])
             mTs <- lift (slv (fmap Mta [Zro .. pred i']) cs)
             let ttas = zip [Zro ..] mTs
             if length [() | Mta _ <- mTs] == 0
               then return (appTtas ttas t)
               else fail "Type Error!"

typChk :: forall n t. (Cnv (TypFld, ()) t , Cnv (t, ()) TypFld) =>
          Exp n t -> ES.Env n t-> NamM ErrM t
typChk e r = do r' :: ES.Env n TypFld <- traverse
                      (flip (curry cnv) ()) r
                e' :: Exp n TypFld <- traverse (flip (curry cnv) ()) e
                t <- chk e' r'
                cnv (t , ())

collect :: Exp n TypFld -> ES.Env n TypFld ->
         InfM (EnvFld '[]) TypFld
collect ee r = case ee of
    ConI _         -> return TH.Int
    ConB _         -> return Bol
    ConF _         -> return Flt
    Var x          -> return (get x r)
    Abs eb         -> do ta <- newMT
                         tb <- collect eb (Ext ta r)
                         return (Arr ta tb)
    App t  ef ea   -> do ta <- collect ea r
                         tf <- collect ef r
                         tb <- newMT
                         addC (tf :~: Arr ta tb)
                         addC (t  :~: ta)
                         return tb
    Tpl ef es      -> TH.Tpl <$> collect ef r <*> collect es r
    Fst t e        -> do te  <- collect e r
                         tf  <- newMT
                         ts  <- newMT
                         addC (te :~: TH.Tpl tf ts)
                         addC (t  :~: ts)
                         return tf
    Snd t  e       -> do te  <- collect e r
                         tf  <- newMT
                         ts  <- newMT
                         addC (te :~: TH.Tpl tf ts)
                         addC (t  :~: tf)
                         return ts
    Let t  el eb   -> do tl  <- collect el r
                         tb  <- collect eb (Ext tl r)
                         addC (t :~: tl)
                         return tb
    Non            -> do t <- newMT
                         return (TH.May t)
    Som e          -> do t   <- collect e r
                         return (TH.May t)
    May t em en es -> do tm  <- collect em r
                         tn  <- collect en r
                         ts  <- collect es r
                         ta  <- newMT
                         addC (tm :~: TH.May ta)
                         addC (ts :~: Arr ta tn)
                         addC (t  :~: ta)
                         return tn
    Typ t e        -> do te <- collect e r
                         addC (t :~: te)
                         return te
    Int _          -> newMT
    Mem e          -> collect e r
    Fix e          -> do te <- collect e r
                         ta <- newMT
                         addC (te :~: Arr ta ta)
                         return ta

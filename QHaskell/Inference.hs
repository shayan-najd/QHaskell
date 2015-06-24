module QHaskell.Inference where

import QHaskell.MyPrelude

import QHaskell.Nat.ADT
import qualified QHaskell.Type.Herbrand as TH
import QHaskell.Type.Herbrand hiding (Tpl,App)
import QHaskell.Solver
import QHaskell.Conversion
import QHaskell.Nat.Conversion ()
import QHaskell.Expression.GADTTyped
import QHaskell.Environment.Scoped  as ES
import qualified QHaskell.Environment.Map  as EM
import QHaskell.InferenceMonad

type TypFld = Typ (EnvFld '[])

mxm :: [Nat] -> Nat
mxm [] = Zro
mxm l  = maximum l

ind :: Exp m n (Maybe TypFld) -> Exp m n TypFld
ind e = (flip evalState
         (succ (maxMta (fmap (maybe (Mta Zro) id) e))) .
         mapM (maybe (do i <- getState
                         put (Suc i)
                         return (Mta i)) return)) e

typInf :: Exp m n (Maybe TypFld) -> (ES.Env m TypFld , ES.Env n TypFld)->
          NamM ErrM (Exp m n TypFld)
typInf e r = inf (ind e) r

maxMta :: Exp m n TypFld -> Nat
maxMta = mxm . concat . fmap mtas . toList

inf :: Exp m n TypFld -> (ES.Env m TypFld , ES.Env n TypFld) -> NamM ErrM (Exp m n TypFld)
inf e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (i' , cs) = execState (collect e r) (succ mts , [])
             mTs <- lift (slv (fmap Mta [Zro .. pred i']) cs)
             let ttas = zip [Zro ..] mTs

             return (fmap (appTtas ttas) e)

chk :: Exp m n TypFld -> (ES.Env m TypFld , ES.Env n TypFld) -> NamM ErrM TypFld
chk e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (t , (i' , cs)) = runState (collect e r) (mts , [])
             mTs <- lift (slv (fmap Mta [Zro .. pred i']) cs)
             let ttas = zip [Zro ..] mTs
             if length [() | Mta _ <- mTs] == 0
               then return (appTtas ttas t)
               else fail "Type Error!"

typChk :: forall m n t. (Cnv (TypFld, ()) t , Cnv (t, ()) TypFld) =>
          Exp m n t -> (ES.Env m t , ES.Env n t) -> NamM ErrM t
typChk e (s , g) = do s' :: ES.Env m TypFld <- traverse (flip (curry cnv) ()) s
                      g' :: ES.Env n TypFld <- traverse (flip (curry cnv) ()) g
                      e' :: Exp m n TypFld <- traverse (flip (curry cnv) ()) e
                      t <- chk e' (s' , g')
                      cnv (t , ())

matchArgs :: TypFld -> [TypFld] -> InfM (EnvFld '[]) TypFld
matchArgs tf []       = return tf
matchArgs tf (ta : ts) = do tb <- newMT
                            addC (tf :~: Arr ta tb)
                            matchArgs tb ts

refresh :: TypFld -> InfM (EnvFld '[]) TypFld
refresh t = do r <- sequence [(,) <$> pure n <*> newMT | n <- (nub (mtas t))]
               refresh' r t

refresh' :: EM.Env Nat (Typ t) -> Typ t -> InfM (EnvFld '[])  (Typ t)
refresh' r (TH.App x as) = TH.App x <$> mapM (refresh' r) as
refresh' r (TH.Mta n)    = EM.get n r

collect :: Exp m n TypFld -> (ES.Env m TypFld , ES.Env n TypFld) ->
           InfM (EnvFld '[]) TypFld
collect ee (s , g) = case ee of
    ConI _         -> return TH.Wrd
    ConB _         -> return Bol
    ConF _         -> return Flt
    Var x          -> return (get x g)
    Prm ts x  es   -> do let tx = get x s
                         tes <- mapM (flip collect (s , g)) es
                         sequence_ (zipWith (\ t te -> addC (t :~: te)) ts tes)
                         tx' <- refresh tx
                         matchArgs tx' tes
    Abs eb         -> do ta <- newMT
                         tb <- collect eb (s , Ext ta g)
                         return (Arr ta tb)
    App t  ef ea   -> do ta <- collect ea (s , g)
                         tf <- collect ef (s , g)
                         tb <- newMT
                         addC (tf :~: Arr ta tb)
                         addC (t  :~: ta)
                         return tb
    Cnd ec et ef   -> do tc <- collect ec (s , g)
                         addC (tc :~: Bol)
                         tt <- collect et (s , g)
                         tf <- collect ef (s , g)
                         addC (tt :~: tf)
                         return tt
    Tpl ef es      -> TH.Tpl <$> collect ef (s , g) <*> collect es (s , g)
    Fst t e        -> do te  <- collect e (s , g)
                         tf  <- newMT
                         ts  <- newMT
                         addC (te :~: TH.Tpl tf ts)
                         addC (t  :~: ts)
                         return tf
    Snd t  e       -> do te  <- collect e (s , g)
                         tf  <- newMT
                         ts  <- newMT
                         addC (te :~: TH.Tpl tf ts)
                         addC (t  :~: tf)
                         return ts
    LeT t  el eb   -> do tl  <- collect el (s , g)
                         tb  <- collect eb (s , Ext tl g)
                         addC (t :~: tl)
                         return tb
    Typ t e        -> do te <- collect e (s , g)
                         addC (t :~: te)
                         return te
    Int _          -> newMT
    Mem e          -> collect e (s , g)
    Fix e          -> do te <- collect e (s , g)
                         ta <- newMT
                         addC (te :~: Arr ta ta)
                         return ta

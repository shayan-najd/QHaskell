{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Utils.GADTFirstOrder
       (sucAll,prdAll,prdAllM,mapVar,sbs,replaceOne,cntVar,pattern TF
       ,isVal,val,pattern V,pattern NV,substitute) where

import QHaskell.MyPrelude
import QHaskell.Expression.GADTFirstOrder as FGFO
import QHaskell.Variable.Typed
import QHaskell.Expression.Utils.Common
import QHaskell.Singleton
import QHaskell.Type.GADT hiding (Int,May,Tpl)

tagFree :: Exp r t -> Exp r t
tagFree (Tag _ e) = tagFree e
tagFree e         = e

pattern TF e <- (tagFree -> e)

sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc

prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar (\(Suc x) -> x)

prdAllM :: Exp (t ': r) t' -> Maybe (Exp r t')
prdAllM = mapVarM (\ v -> case v of
                            Zro -> Nothing
                            Suc x -> Just x)


mapVarM :: forall r r' t m. (Applicative m , Monad m) =>
          (forall t'. Var r t' -> m (Var r' t')) -> Exp r t -> m (Exp r' t)
mapVarM f ee = case ee of
 Var v -> Var <$> f v
 _     -> $(genOverloadedM 'ee ''Exp  ['Var]
  (\ t -> if
      | matchQ t [t| Exp (t ': t) t |] -> [| mapVarM (incM f) |]
      | matchQ t [t| Exp t t |]        -> [| mapVarM f  |]
      | otherwise                      -> [| pure |]))


mapVar :: forall r r' t.
          (forall t'. Var r t' -> Var r' t') -> Exp r t -> Exp r' t
mapVar f ee = case ee of
 Var v -> Var (f v)
 _     -> $(genOverloaded 'ee ''Exp  ['Var]
  (\ t -> if
      | matchQ t [t| Exp (t ': t) t |] -> [| mapVar (inc f) |]
      | matchQ t [t| Exp t t |]        -> [| mapVar f  |]
      | otherwise                      -> [| id |]))

deriving instance Show (Exp g a)

substitute :: Exp g a -> Exp (a ': g) b -> Exp g b
substitute = sbs

sbs :: Exp r a -> Exp (a ': r) b -> Exp r b
sbs e' ee = prdAll (sbs' (sucAll e') Zro ee)

rp :: Var (a ': g) t -> Var (a ': b ': g) t
rp v = case v of
         Zro    -> Zro
         Suc v' -> Suc (Suc v')

replaceOne :: Exp (tl ': n) t -> Exp (tl ': tl1 ': n) t
replaceOne = mapVar rp

cntVar :: forall r t t'. (HasSin Typ t' , HasSin Typ t) =>
          Var r t' -> Exp r t -> Int
cntVar v ee = let t = sin :: Typ t in case ee of
  Var x     -> case eqlSin t (sinTyp v) of
    Rgt Rfl -> if x == v
               then 1
               else 0
    _       -> 0
  _         -> $(recAppMQ 'ee ''Exp (const [| (0 :: Int) |]) ['Var]
    [| \ _x -> 0 |] [| (+) |] [| (+) |] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp (t ': t) t |]     -> [| cntVar (Suc v) |]
    | matchQ tt [t| Exp t t |]            -> [| cntVar v |]
    | otherwise                           -> [| const 0  |]))

sbs' :: forall r t t'.
        Exp r t' -> Var r t' -> Exp r t -> Exp r t
sbs' e' v' ee = case ee of
  Var v -> case eqlVar v v' of
     Just Rfl -> e'
     Nothing  -> ee
  _   -> $(genOverloaded 'ee ''Exp  ['Var]
   (\ tt -> if
      | matchQ tt [t| Exp (t ': t) t |] -> [| sbs'F e' v' |]
      | matchQ tt [t| Exp t t |]        -> [| sbs'  e' v' |]
      | otherwise                       -> [| id |]))

sbs'F :: forall r t t' t''.
         Exp r t' -> Var r t' -> Exp (t'' ': r) t -> Exp (t'' ': r) t
sbs'F e' v' e = sbs' (sucAll e') (Suc v') e

isVal :: Exp n t -> Bool
isVal ee = case ee of
    ConI _        -> True
    ConB _        -> True
    ConF _        -> True
    Var  _        -> True
    Abs  _        -> True
    App  _  _     -> False
    Tpl  ef es    -> isVal ef && isVal es
    Fst  _        -> False
    Snd  _        -> False
    Let  _  _     -> False
    Non           -> True
    Som  e        -> isVal e
    May  _ _  _   -> False
    Int _         -> True -- shouldn't matter
    Tag _ e       -> isVal e
    Mem _         -> False
    Fix _         -> False

val :: Exp n t -> (Bool,Exp n t)
val ee = (isVal ee , ee)

pattern V  v <- (val -> (True  , v))
pattern NV v <- (val -> (False , v))

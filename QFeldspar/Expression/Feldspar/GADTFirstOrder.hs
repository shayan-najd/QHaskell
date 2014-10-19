module QFeldspar.Expression.Feldspar.GADTFirstOrder
    (Exp(..),sucAll,prdAll,mapVar) where

import QFeldspar.MyPrelude

import QFeldspar.Variable.Typed

import QFeldspar.Singleton

import qualified QFeldspar.Type.Feldspar.GADT as TFG

data Exp :: [*] -> * -> * where
  ConI :: Int      -> Exp r Int
  ConB :: Bool     -> Exp r Bol
  ConF :: Float    -> Exp r Flt
  Var  :: Var r t  -> Exp r t
  Abs  :: Exp (ta ': r) tb -> Exp r (Arr ta tb)
  App  :: HasSin TFG.Typ ta =>
          Exp r (Arr ta tb) -> Exp r ta -> Exp r tb
  Cnd  :: Exp r Bol -> Exp r t -> Exp r t -> Exp r t
  Whl  :: Exp (t ': r) Bol -> Exp (t ': r) t -> Exp r t -> Exp r t
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (Tpl tf ts)
  Fst  :: HasSin TFG.Typ ts => Exp r (Tpl tf ts) -> Exp r tf
  Snd  :: HasSin TFG.Typ tf => Exp r (Tpl tf ts) -> Exp r ts
  Ary  :: Exp r Int -> Exp (Int ': r) t -> Exp r (Ary t)
  Len  :: HasSin TFG.Typ ta => Exp r (Ary ta) -> Exp r Int
  Ind  :: Exp r (Ary ta) -> Exp r Int -> Exp r ta
  Let  :: HasSin TFG.Typ tl => Exp r tl -> Exp (tl ': r) tb -> Exp r tb
  Cmx  :: Exp r Flt -> Exp r Flt -> Exp r Cmx
  Non  :: Exp r (May tl)
  Som  :: Exp r tl -> Exp r (May tl)
  May  :: HasSin TFG.Typ a =>
          Exp r (May a) -> Exp r b -> Exp (a ': r) b -> Exp r b

sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc

prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar (\(Suc x) -> x)

mapVar :: forall r r' t.
          (forall t'. Var r t' -> Var r' t') -> Exp r t -> Exp r' t
mapVar f ee = case ee of
  ConI i       -> ConI i
  ConB i       -> ConB i
  ConF i       -> ConF i
  Var v        -> Var (f v)
  Abs eb       -> Abs (mf eb)
  App ef ea    -> App (m ef)  (m ea)
  Cnd ec et ef -> Cnd (m ec)  (m et)  (m ef)
  Whl ec eb ei -> Whl (mf ec) (mf eb) (m ei)
  Tpl ef es    -> Tpl (m ef)  (m es)
  Fst e        -> Fst (m e)
  Snd e        -> Snd (m e)
  Ary el ef    -> Ary (m el)  (mf ef)
  Len e        -> Len (m e)
  Ind ea ei    -> Ind (m ea)  (m ei)
  Let el eb    -> Let (m el)  (mf eb)
  Cmx er ei    -> Cmx (m er)  (m ei)
  Non          -> Non
  Som e        -> Som (m e)
  May ec en es -> May (m ec)  (m en) (mf es)
  where
    m :: Exp r tt -> Exp r' tt
    m  = mapVar f

    mf :: Exp (ta ': r) tt -> Exp (ta ': r') tt
    mf = mapVar (inc f)

{-
sbs :: forall r t t'.Exp r t' -> Var r t -> Exp r t -> Exp r t'
sbs ebb v eaa = case ebb of
  ConI i       -> ConI i
  ConB i       -> ConB i
  Var x
    | x == v     -> eaa
    | otherwise  -> ebb
  Abs eb       -> Abs (mf eb)
  App ef ea    -> App (m ef)  (m ea)
  Cnd ec et ef -> Cnd (m ec)  (m et)  (m ef)
  Whl ec eb ei -> Whl (mf ec) (mf eb) (m ei)
  Tpl ef es    -> Tpl (m ef)  (m es)
  Fst e        -> Fst (m e)
  Snd e        -> Snd (m e)
  Ary el ef    -> Ary (m el)  (mf ef)
  Len e        -> Len (m e)
  Ind ea ei    -> Ind (m ea)  (m ei)
  Let el eb    -> Let (m el)  (mf eb)
  where
    m :: forall t'. Exp r t' -> Exp r t'
    m  e = sbs e v eaa

    mf :: forall t' tt. Exp (tt ': r) t' -> Exp (tt ': r) t'
    mf e = sbs e (Suc v) (sucAll eaa)
-}
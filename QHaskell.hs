{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module QHaskell
       (module Prelude,
        norm,tran,tranQ,eval,Qt,Dp,Type,EvalEnv,TypeEnv,(<:>),(<+>),nil,NameType(..),
        ErrM(..),frmRgt)
where

import Prelude
-- import Prelude(Float,Bool(..),Maybe,String,(.))
import QHaskell.MyPrelude (ErrM(..),runNamM,frmRgtZro,frmRgt)

import QHaskell.Singleton
import QHaskell.Conversion
import QHaskell.Normalisation
import qualified QHaskell.Environment.Typed          as ET
import qualified QHaskell.Environment.Scoped         as ES
import qualified Language.Haskell.TH.Syntax      as TH
import qualified QHaskell.Expression.GADTFirstOrder  as GFO
import qualified QHaskell.Expression.GADTValue       as GV
import qualified QHaskell.Type.GADT                  as TG
import QHaskell.Expression.Utils.TemplateHaskell
import QHaskell.Expression.Conversions.Reification
import QHaskell.Expression.Conversions.Evaluation.GADTFirstOrder ()
import QHaskell.Expression.Conversion ()
import QHaskell.Type.Conversion ()
import QHaskell.Normalisation ()

data NameType a = NameType {name :: TH.Name , singType :: TG.Typ a}

infixr 5 <:>
(<:>) :: Type a => TH.Name -> TypeEnv g -> TypeEnv (a ': g)
n <:> g = ET.Ext (NameType (stripNameSpace n) QHaskell.Singleton.sin) g

infixr 5 <+>
(<+>) :: Type a => a -> EvalEnv g -> EvalEnv (a ': g)
v <+> g = ET.Ext (GV.Exp v) g

nil :: ET.Env tg '[]
nil  = ET.Emp

type TypeEnv g  = ET.Env NameType g
type EvalEnv g  = ET.Env GV.Exp   g
type Type a     = HasSin TG.Typ a
type Dp g a     = GFO.Exp g a
type Qt a       = TH.Q (TH.TExp a)

norm :: Type a => Dp g a -> Dp g a
norm = nrm

tran :: Type a => TypeEnv g -> Qt a -> ErrM (Dp g a)
tran g q = let (ets ,ess) = decompose g
           in runNamM (cnv (q , ets , ess))

tranQ :: Type a => TypeEnv g -> Qt a -> Qt (Dp g a)
tranQ g q = do let (ets ,ess) = decompose g
               case runNamM (cnv (q , ets , ess)) of
                 Rgt d -> rei d
                 Lft s -> fail s

eval :: Type a => EvalEnv g -> Dp g a -> a
eval g d = let GV.Exp v = frmRgtZro (cnv (d , g))
           in  v

decompose :: TypeEnv g -> (ET.Env TG.Typ g , ES.Env (Len g) TH.Name)
decompose ET.Emp        = (ET.Emp , ES.Emp)
decompose (ET.Ext n ns) = let (ets , ess) = decompose ns
                          in  ( ET.Ext (singType n) ets
                              , ES.Ext (name n) ess)


{-

translate :: forall a.
             (Type a) =>
             Qt a -> Dp a
translate f = frmRgtZro (cnv (wrp f , etTG , esTH))

translateF :: forall a b.
             (Type a , Type b) =>
             Qt (a -> b) -> Dp a -> Dp b
translateF f x = FMWS.absVar
                 (frmRgtZro
                 (cnv (wrp
                 ([|| $$f $$dummy ||])
                 , (sin :: TG.Typ a) <:> etTG
                 , dn <+> esTH))) x

evaluate ::  forall a.
             (Type a) =>
             Qt a -> a
evaluate = CDSL.evaluate . translate
-}

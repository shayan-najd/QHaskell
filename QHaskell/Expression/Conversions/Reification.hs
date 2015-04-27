module QHaskell.Expression.Conversions.Reification(rei,reiVar)  where

import QHaskell.Variable.Typed                             as VT
import qualified QHaskell.Expression.GADTFirstOrder        as FGFO
import QHaskell.Variable.Conversion ()

import qualified Language.Haskell.TH.Syntax as TH

reiVar :: Var g a -> TH.Q (TH.TExp (Var g a))
reiVar x = [|| x ||]

rei :: FGFO.Exp g a -> TH.Q (TH.TExp (FGFO.Exp g a))
rei ee = [|| ee ||]

instance TH.Lift (Var g a) where
 lift x = case x of
  Zro   -> [| Zro |]
  Suc y -> [| Suc y |]

instance TH.Lift (FGFO.Exp g a) where
 lift ee = case ee of
  FGFO.ConB b    -> [| FGFO.ConB b |]
  FGFO.ConF f    -> [| FGFO.ConF f |]
  FGFO.ConI i    -> [| FGFO.ConI i |]
  FGFO.Var x     -> [| FGFO.Var  x |]
  FGFO.Abs n     -> [| FGFO.Abs  n |]
  FGFO.App l m   -> [| FGFO.App  l m |]
  FGFO.Tpl m n   -> [| FGFO.Tpl  m n |]
  FGFO.Fst l     -> [| FGFO.Fst  l |]
  FGFO.Snd l     -> [| FGFO.Snd  l |]
  FGFO.Let m n   -> [| FGFO.Let  m n |]
  FGFO.Non       -> [| FGFO.Non |]
  FGFO.Som m     -> [| FGFO.Som  m |]
  FGFO.May l m n -> [| FGFO.May  l m n |]
  FGFO.Int i     -> [| FGFO.Int  i |]
  FGFO.Tag s m   -> [| FGFO.Tag  s m |]
  FGFO.Mem m     -> [| FGFO.Mem  m |]
  FGFO.Fix m     -> [| FGFO.Fix  m |]

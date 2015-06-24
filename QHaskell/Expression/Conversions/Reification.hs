module QHaskell.Expression.Conversions.Reification(rei,reiVar)  where

import QHaskell.Variable.Typed                             as VT
import QHaskell.Environment.Typed
import qualified QHaskell.Expression.GADTFirstOrder        as GFO
import QHaskell.Variable.Conversion ()

import qualified Language.Haskell.TH.Syntax as TH

reiVar :: Var g a -> TH.Q (TH.TExp (Var g a))
reiVar x = [|| x ||]

rei :: GFO.Exp s g a -> TH.Q (TH.TExp (GFO.Exp s g a))
rei ee = [|| ee ||]

instance TH.Lift (Var g a) where
 lift x = case x of
  Zro   -> [| Zro |]
  Suc y -> [| Suc y |]

instance TH.Lift (Env (GFO.Exp s g) d) where
 lift g = case g of
   Emp      -> [| Emp |]
   Ext x xs -> [| Ext x xs |]

instance TH.Lift (GFO.Exp s g a) where
 lift ee = case ee of
  GFO.ConB b    -> [| GFO.ConB b |]
  GFO.ConF f    -> [| GFO.ConF f |]
  GFO.ConI i    -> [| GFO.ConI i |]
  GFO.Prm x ms  -> [| GFO.Prm x ms |]
  GFO.Cnd l m n -> [| GFO.Cnd  l m n |]
  GFO.Var x     -> [| GFO.Var  x |]
  GFO.Abs n     -> [| GFO.Abs  n |]
  GFO.App l m   -> [| GFO.App  l m |]
  GFO.Tpl m n   -> [| GFO.Tpl  m n |]
  GFO.Fst l     -> [| GFO.Fst  l |]
  GFO.Snd l     -> [| GFO.Snd  l |]
  GFO.LeT m n   -> [| GFO.LeT  m n |]
  GFO.Int i     -> [| GFO.Int  i |]
  GFO.Tag s m   -> [| GFO.Tag  s m |]
  GFO.Mem m     -> [| GFO.Mem  m |]
  GFO.Fix m     -> [| GFO.Fix  m |]

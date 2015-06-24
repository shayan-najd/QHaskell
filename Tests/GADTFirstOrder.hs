module Tests.GADTFirstOrder where

import QHaskell.MyPrelude

import QHaskell.Expression.GADTFirstOrder
import QHaskell.Variable.Typed
import QHaskell.Conversion
import QHaskell.Expression.Conversions.Evaluation.GADTFirstOrder ()
import qualified QHaskell.Expression.GADTValue as FGV
import QHaskell.Environment.Typed

import QHaskell.Type.GADT

dbl :: Exp '[Word32 -> Word32 -> Word32] '[] (Word32 -> Word32)
dbl = -- Abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))
       Abs (Prm Zro (Ext (Var Zro) (Ext (Var Zro) Emp)))

compose :: (Type ta , Type tb , Type tc) =>
           Exp s g ((tb -> tc) -> ((ta -> tb) -> (ta -> tc)))
compose = Abs (Abs (Abs
                    (App (Var (Suc (Suc Zro)))
                     (App (Var (Suc Zro)) (Var Zro)))))

four :: Exp '[Word32 -> Word32 -> Word32] '[] Word32
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = case runNamM (cnv (four
                , (Ext (FGV.Exp (+) :: FGV.Exp (Word32 -> Word32 -> Word32)) Emp, Emp :: Env FGV.Exp '[])))
       of
  Rgt (FGV.Exp x) -> x == 4
  Lft _           -> False

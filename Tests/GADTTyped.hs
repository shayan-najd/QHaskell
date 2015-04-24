module Tests.GADTTyped where

import QHaskell.MyPrelude

import QHaskell.Expression.GADTTyped
import QHaskell.Variable.Scoped
import qualified QHaskell.Type.ADT as TFA
import qualified QHaskell.Expression.ADTValue as V
import qualified QHaskell.Nat.ADT as NA
import QHaskell.Environment.Scoped
import QHaskell.Conversion
import QHaskell.Expression.Conversions.Evaluation.GADTTyped ()
import QHaskell.Inference

dbl :: Exp (NA.Suc NA.Zro) TFA.Typ
dbl = Abs (App TFA.Int (App TFA.Int (Var (Suc Zro)) (Var Zro)) (Var Zro))

compose :: TFA.Typ -> TFA.Typ -> Exp n TFA.Typ
compose ta tb = Abs (Abs (Abs
  (App tb (Var (Suc (Suc Zro)))
   (App ta (Var (Suc Zro)) (Var Zro)))))

four :: Exp (NA.Suc NA.Zro) TFA.Typ
four = App TFA.Int
       (App (TFA.Arr TFA.Int TFA.Int)
        (App (TFA.Arr TFA.Int TFA.Int)
         (compose TFA.Int TFA.Int) dbl) dbl) (ConI 1)

test :: Bool
test = (case runNamM (cnv (four , (Ext (V.lft ((+) :: Int -> Int -> Int)) Emp))) of
          Rgt (V.colft -> Rgt (4 :: Int)) -> True
          _                           -> False)
       && (runNamM (typChk four (Ext (TFA.Arr TFA.Int
                             (TFA.Arr TFA.Int TFA.Int)) Emp))
           ==
           Rgt TFA.Int)

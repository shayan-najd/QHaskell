module Tests.GADTFirstOrder where

import QHaskell.MyPrelude

import QHaskell.Expression.GADTFirstOrder
import QHaskell.Variable.Typed
import QHaskell.Conversion
import QHaskell.Expression.Conversions.Evaluation.GADTFirstOrder ()
import qualified QHaskell.Expression.GADTValue as FGV
import QHaskell.Singleton
import QHaskell.Environment.Typed

import qualified QHaskell.Type.GADT as TFG

dbl :: Exp (Arr Int (Arr Int Int) ': '[])
       (Arr Int Int)
dbl = Abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))

compose :: (HasSin TFG.Typ ta , HasSin TFG.Typ tb , HasSin TFG.Typ tc) =>
           Exp r (Arr (Arr tb tc) (Arr (Arr ta tb)
                   (Arr ta tc)))
compose = Abs (Abs (Abs
                    (App (Var (Suc (Suc Zro)))
                     (App (Var (Suc Zro)) (Var Zro)))))

four :: Exp (Arr Int (Arr Int Int) ': '[]) Int
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = case runNamM (cnv (four
                , Ext
                  (FGV.Exp (+)
                   :: FGV.Exp (Arr Int (Arr Int Int))) Emp))
       of
  Rgt (FGV.Exp x) -> x == 4
  Lft _           -> False

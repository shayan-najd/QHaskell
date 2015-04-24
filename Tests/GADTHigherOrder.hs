module Tests.GADTHigherOrder where

import QHaskell.MyPrelude

import QHaskell.Expression.GADTHigherOrder
import QHaskell.Variable.Typed
import QHaskell.Conversion as E
import QHaskell.Expression.Conversions.Evaluation.GADTHigherOrder ()
import qualified QHaskell.Expression.GADTValue as FGV
import QHaskell.Singleton
import QHaskell.Type.GADT
import QHaskell.Environment.Typed

dbl :: Exp (Arr Int (Arr Int Int) ': '[])
       (Arr Int Int)
dbl = Abs (\ x -> (App (App (Var Zro) x) x))

compose :: (HasSin Typ ta , HasSin Typ tb , HasSin Typ tc) =>
           Exp r (Arr (Arr tb tc) (Arr (Arr ta tb)
                                           (Arr ta tc)))
compose = Abs (\ g -> Abs (\ f -> Abs
                    (\ x -> App g (App f x))))

four :: Exp (Arr Int (Arr Int Int) ': '[]) Int
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = case runNamM (cnv ( four
                , Ext (FGV.Exp (+)
                       :: FGV.Exp (Arr Int (Arr Int Int)))
                  Emp)) of
  Rgt (FGV.Exp x) -> x == 4
  Lft _           -> False

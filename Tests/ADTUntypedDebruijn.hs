module Tests.ADTUntypedDebruijn where

import QHaskell.MyPrelude

import QHaskell.Expression.ADTUntypedDebruijn
import QHaskell.Variable.Plain
import qualified QHaskell.Expression.ADTValue as V
import QHaskell.Conversion
import QHaskell.Expression.Conversions.Evaluation.ADTUntypedDebruijn ()

dbl :: Exp
dbl = Abs (Fun (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro)))

compose :: Exp
compose = Abs (Fun
            (Abs (Fun
               (Abs (Fun (App (Var (Suc (Suc Zro)))
                                  (App (Var (Suc Zro))
                                           (Var Zro))))))))

four :: Exp
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = (case runNamM (cnv (four , [V.lft ((+) :: Int -> Int -> Int)])) of
          Rgt (V.colft -> Rgt (4 :: Int)) -> True
          _                               -> False)

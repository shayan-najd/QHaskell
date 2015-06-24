module Tests.GADTHigherOrder where

import QHaskell.MyPrelude

import QHaskell.Expression.GADTHigherOrder
import QHaskell.Variable.Typed
import QHaskell.Conversion as E
import QHaskell.Expression.Conversions.Evaluation.GADTHigherOrder ()
import qualified QHaskell.Expression.GADTValue as FGV
import QHaskell.Type.GADT
import QHaskell.Environment.Typed

dbl :: Exp '[Word32 -> Word32 -> Word32] (Word32 -> Word32)
dbl = Abs (\ x -> Prm Zro (x `Ext` (x `Ext` Emp)))

compose :: (Type ta , Type tb , Type tc) =>
           Exp r ((tb -> tc) -> ((ta -> tb) -> (ta -> tc)))
compose = Abs (\ g -> Abs (\ f -> Abs
                    (\ x -> App g (App f x))))

four :: Exp '[Word32 -> Word32 -> Word32] Word32
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = case runNamM (cnv ( four
                , Ext (FGV.Exp (+)
                       :: FGV.Exp (Word32 -> Word32 -> Word32))
                  Emp)) of
  Rgt (FGV.Exp x) -> x == 4
  Lft _           -> False

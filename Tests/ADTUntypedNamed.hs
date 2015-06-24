module Tests.ADTUntypedNamed where

import QHaskell.MyPrelude
import QHaskell.Expression.ADTUntypedNamed
import qualified QHaskell.Expression.ADTValue as V
import QHaskell.Conversion
import QHaskell.Expression.Conversions.Evaluation.ADTUntypedNamed ()
import qualified Language.Haskell.TH.Syntax as TH
import QHaskell.Expression.Utils.TemplateHaskell
import Tests.TemplateHaskell(add)
type Var = TH.Name

x0 :: Var
x0 = TH.mkName  "x0"

x1 :: Var
x1 = TH.mkName  "x1"

x2 :: Var
x2 = TH.mkName  "x2"

dbl :: Exp Var
dbl = Abs (x0 , Prm (stripNameSpace 'add) [Var x0 , Var x0])

compose :: Exp Var
compose = Abs (x2 , (Abs (x1 , (Abs (x0 , (App (Var x2) (App (Var x1) (Var x0))))))))

four :: Exp Var
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = (case runNamM (cnv (four , ([((stripNameSpace 'add)
                     , V.lft ((+) :: Word32 -> Word32 -> Word32))]
                     ,[] :: [(TH.Name,V.Exp)]))) of
          Rgt (V.colft -> Rgt (4 :: Word32)) -> True
          _                               -> False)

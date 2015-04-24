module QHaskell.Expression.Utils.Equality.GADTHigherOrder (eql,eqlF) where

import QHaskell.MyPrelude

import QHaskell.Expression.GADTHigherOrder
import QHaskell.Conversion
import QHaskell.Environment.Typed
import QHaskell.Singleton
import QHaskell.Expression.Conversions.Lifting (cnvHOFOF)
import qualified QHaskell.Type.GADT as TFG
import qualified QHaskell.Expression.GADTFirstOrder as GFO
import qualified QHaskell.Expression.Utils.Equality.GADTFirstOrder as GFO

eql :: forall a g.
       (HasSin TFG.Typ a , HasSin (Env TFG.Typ) g) =>
       Exp g a -> Exp g a -> Bool
eql e1 e2 = let g = sin :: Env TFG.Typ g
                e1' :: GFO.Exp g a = frmRgtZro (cnv (e1 , g))
                e2' :: GFO.Exp g a = frmRgtZro (cnv (e2 , g))
            in GFO.eql e1' e2'

eqlF :: forall a b g.
        (HasSin TFG.Typ a , HasSin TFG.Typ b , HasSin (Env TFG.Typ) g) =>
        (Exp g a -> Exp g b) -> (Exp g a -> Exp g b) -> Bool
eqlF f1 f2 = let g = sin :: Env TFG.Typ g
                 f1' :: GFO.Exp (a ': g) b = cnvHOFOF g f1
                 f2' :: GFO.Exp (a ': g) b = cnvHOFOF g f2
             in GFO.eql f1' f2'

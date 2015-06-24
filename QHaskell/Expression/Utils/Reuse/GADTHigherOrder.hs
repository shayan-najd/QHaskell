module QHaskell.Expression.Utils.Reuse.GADTHigherOrder where

import QHaskell.MyPrelude
import QHaskell.Expression.GADTHigherOrder
import QHaskell.Environment.Typed
import QHaskell.Conversion
import QHaskell.Singleton
import qualified QHaskell.Type.GADT as TG
import qualified QHaskell.Expression.GADTFirstOrder as GFO
import QHaskell.Expression.Conversions.Lifting (cnvHOFOF,cnvFOHOF)

onHOAS :: forall s a.
          (TG.Type a , TG.Types s) =>
          (TG.Type a => GFO.Exp s '[] a -> GFO.Exp s '[] a) ->
          Exp s a -> Exp s a
onHOAS f e =  let g = sin :: Env TG.Typ s
                  e' :: GFO.Exp s '[] a = frmRgtZro (cnv (e , g))
              in frmRgtZro (cnv (f e' , g))

onHOASF :: forall s a b.
          (TG.Type a , TG.Type b , TG.Types s) =>
          ((TG.Type a , TG.Type b) =>
           GFO.Exp s '[a] b -> GFO.Exp s '[a] b) ->
          (Exp s a -> Exp s b) -> Exp s a -> Exp s b
onHOASF f e =  let g = sin :: Env TG.Typ s
                   e' :: GFO.Exp s '[a] b = cnvHOFOF g e
               in cnvFOHOF (f e')

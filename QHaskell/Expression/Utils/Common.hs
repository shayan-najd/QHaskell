module QHaskell.Expression.Utils.Common where
import QHaskell.MyPrelude
import QHaskell.Type.GADT
import QHaskell.Singleton
import QHaskell.Expression.Utils.TemplateHaskell
import qualified Language.Haskell.TH as TH

trvWrp :: TH.Name -> TH.Name -> TH.ExpQ -> TH.ExpQ
trvWrp t =  (\ n e -> if
       | n === TH.mkName "Abs"  ->
           [| case getPrfHasSinArr $(TH.varE t) of
                (PrfHasSin , PrfHasSin) -> $e |]
       | n === TH.mkName "Tpl"  ->
           [| case getPrfHasSinTpl $(TH.varE t) of
                (PrfHasSin , PrfHasSin) -> $e |]
       | otherwise   -> e)

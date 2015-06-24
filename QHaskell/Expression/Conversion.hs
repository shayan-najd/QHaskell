module QHaskell.Expression.Conversion () where

import QHaskell.MyPrelude

import QHaskell.Conversion
import QHaskell.Singleton
import QHaskell.Nat.ADT
import qualified QHaskell.Nat.GADT as NG
import qualified Language.Haskell.TH.Syntax as TH
import qualified QHaskell.Expression.ADTUntypedNamed as AUN
import qualified QHaskell.Expression.ADTUntypedDebruijn as AUD
import qualified QHaskell.Expression.GADTTyped as GTD
import qualified QHaskell.Expression.GADTFirstOrder as GFO
import qualified QHaskell.Expression.GADTHigherOrder as GHO
import qualified QHaskell.Type.ADT as TA
import qualified QHaskell.Type.GADT as TG
import qualified QHaskell.Environment.Plain as EP
import qualified QHaskell.Environment.Scoped as ES
import qualified QHaskell.Environment.Typed as ET
import QHaskell.Variable.Conversion ()
import QHaskell.Environment.Conversion ()
import QHaskell.Type.Conversion ()
import QHaskell.Expression.Conversions.Unquoting ()
import QHaskell.Expression.Conversions.NameResolution ()
import QHaskell.Expression.Conversions.ScopeWithnessing ()
import QHaskell.Expression.Conversions.TypeInference ()
import QHaskell.Expression.Conversions.TypeWithnessing ()
import QHaskell.Expression.Conversions.Lifting ()
import QHaskell.Expression.Conversions.EtaPrims(etaPrms)

-----------------------------------------------------------------------
-- Conversion from TH.TExp
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GFO.Exp s '[] a <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: GTD.Exp n Zro TA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ n' , n ~ Len s , TG.Type a)  =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GTD.Exp n' Zro TA.Typ)
         where
  cnv (e , s , v) = do e' :: AUD.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s , TG.Type a) =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             AUD.Exp
         where
  cnv (e , s , v) = do e' :: AUN.Exp TH.Name <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             (AUN.Exp TH.Name)
         where
  cnv (e , s , v) = do e' :: TH.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Q (TH.TExp a) , es , en)
             TH.Exp
         where
  cnv (e , _ , _) = lift (TH.runQ (TH.unTypeQ e))

instance (a ~ a' , n ~ Len s) =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             (TH.Q (TH.TExp a'))
         where
  cnv (e , _ , _ ) = pure e

-----------------------------------------------------------------------
-- Conversion from TH.Exp
-----------------------------------------------------------------------
instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (TH.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GFO.Exp s '[] a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (TH.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: GTD.Exp n Zro TA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ n' , n ~ Len s)  =>
         Cnv (TH.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GTD.Exp n' Zro TA.Typ)
         where
  cnv (e , s , v) = do e' :: AUD.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             AUD.Exp
         where
  cnv (e , s , v) = do e' :: AUN.Exp TH.Name <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (AUN.Exp TH.Name)
         where
  cnv (e , s , g) = do e' :: AUN.Exp TH.Name <- cnv (e , ())
                       etaPrms s g e'

instance Cnv (TH.Exp , es , en)
             TH.Exp
         where
  cnv (e , _ , _) = pure e

-----------------------------------------------------------------------
-- Conversion from AUN
-----------------------------------------------------------------------
instance (s ~ s' , t ~ t' , n ~ Len s , TG.Type a') =>
         Cnv (AUN.Exp TH.Name , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GFO.Exp s' '[] a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (AUN.Exp TH.Name , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: GTD.Exp n Zro TA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s)  =>
         Cnv (AUN.Exp TH.Name , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GTD.Exp n Zro TA.Typ)
         where
  cnv (e , s , v) = do e' :: AUD.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance Cnv (AUN.Exp TH.Name , es , ES.Env n TH.Name)
             AUD.Exp
         where
  cnv (e , _ , vs) = do vs' :: EP.Env TH.Name <- cnv (vs , ())
                        cnv (e , (vs' , [] :: EP.Env TH.Name))

instance Cnv (AUN.Exp TH.Name , es , en)
             (AUN.Exp TH.Name)
         where
  cnv (e , _ , _ ) = pure e

-----------------------------------------------------------------------
-- Conversion from AUD
-----------------------------------------------------------------------
instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (AUD.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GFO.Exp s '[] a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (AUD.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: GTD.Exp n Zro TA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n' ~ Len s) =>
         Cnv (AUD.Exp , ET.Env TG.Typ s , en)
             (GTD.Exp n' Zro TA.Typ)
         where
  cnv (e , s , _) = do e' :: GTD.Exp n' Zro (Maybe TA.Typ) <- cnv (e , (ET.len s , NG.Zro))
                       s' :: ES.Env   n' TA.Typ <- cnv (s , ())
                       cnv (e' , (s', ES.Emp :: ES.Env Zro TA.Typ))

instance Cnv (AUD.Exp , es , en)
             AUD.Exp
         where
  cnv (e , _ , _ ) = pure e

-----------------------------------------------------------------------
-- Conversion from GTD
-----------------------------------------------------------------------
instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (GTD.Exp n Zro TA.Typ , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GFO.Exp s '[] t <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (GTD.Exp n Zro TA.Typ , ET.Env TG.Typ s , en)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , _) = cnv (e , (s , ET.Emp :: ET.Env TG.Typ '[]))

instance (n ~ n') =>
         Cnv (GTD.Exp n  Zro TA.Typ , es , en)
             (GTD.Exp n' Zro TA.Typ)
         where
  cnv (e , _ , _) = return e

-----------------------------------------------------------------------
-- Conversion from GFO
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (GFO.Exp s '[] a , ET.Env TG.Typ s , en)
             (GHO.Exp s' a')
         where
  cnv (e , s , _) = cnv ({- nrm -} e , s)

instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (GFO.Exp s  '[] a , es , en)
             (GFO.Exp s' '[] a')
         where
  cnv (e , _ , _) = pure e

-----------------------------------------------------------------------
-- Conversion from GHO
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , TG.Type a') =>
         Cnv (GHO.Exp s  a , es , en)
             (GHO.Exp s' a')
         where
  cnv (e , _ , _) = pure e

instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (GHO.Exp s  a , ET.Env TG.Typ s , en)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , _) = cnv (e , s)

module QHaskell.Expression.Conversion () where

import QHaskell.MyPrelude

import QHaskell.Conversion
import QHaskell.Singleton
-- import QHaskell.Normalisation
-- import QHaskell.Eta
import qualified Language.Haskell.TH.Syntax              as TH
import qualified QHaskell.Expression.ADTUntypedNamed    as FAUN
import qualified QHaskell.Expression.ADTUntypedDebruijn as FAUD
import qualified QHaskell.Expression.GADTTyped          as FGTD
import qualified QHaskell.Expression.GADTFirstOrder     as FGFO
import qualified QHaskell.Expression.GADTHigherOrder    as FGHO
import qualified QHaskell.Type.ADT                      as TFA
import qualified QHaskell.Type.GADT                     as TFG
import qualified QHaskell.Environment.Plain             as EP
import qualified QHaskell.Environment.Scoped            as ES
import qualified QHaskell.Environment.Typed             as ET
import QHaskell.Variable.Conversion ()
import QHaskell.Environment.Conversion ()
import QHaskell.Type.Conversion ()
import QHaskell.Expression.Conversions.Unquoting ()
import QHaskell.Expression.Conversions.NameResolution ()
import QHaskell.Expression.Conversions.ScopeWithnessing ()
import QHaskell.Expression.Conversions.TypeInference ()
import QHaskell.Expression.Conversions.TypeWithnessing ()
import QHaskell.Expression.Conversions.Lifting ()


---------------------------------------------------------------------------------
-- Conversion from TH.TExp
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t' , t ~ tt) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGFO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t', t ~ tt) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGTD.Exp n TFA.Typ <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r , HasSin TFG.Typ tt)  =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGTD.Exp n TFA.Typ)
         where
  cnv (e , r , v) = do e' :: FAUD.Exp <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r , HasSin TFG.Typ tt) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             FAUD.Exp
         where
  cnv (e , r , v) = do e' :: FAUN.Exp TH.Name <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r , HasSin TFG.Typ tt) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FAUN.Exp TH.Name)
         where
  cnv (e , r , v) = do e' :: TH.Exp <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r , HasSin TFG.Typ tt) =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             TH.Exp
         where
  cnv (e , _ , _) = lift (TH.runQ (TH.unTypeQ e))

instance (n ~ Len r , HasSin TFG.Typ tt , tt ~ tt') =>
         Cnv (TH.Q (TH.TExp tt) , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (TH.Q (TH.TExp tt'))
         where
  cnv (e , _ , _ ) = pure e

---------------------------------------------------------------------------------
-- Conversion from TH.Exp
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGFO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGTD.Exp n TFA.Typ <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r)  =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGTD.Exp n TFA.Typ)
         where
  cnv (e , r , v) = do e' :: FAUD.Exp <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r) =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             FAUD.Exp
         where
  cnv (e , r , v) = do e' :: FAUN.Exp TH.Name <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r) =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FAUN.Exp TH.Name)
         where
  cnv (e , _ , _) = cnv (e , ())

instance (n ~ Len r) =>
         Cnv (TH.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             TH.Exp
         where
  cnv (e , _ , _) = pure e

---------------------------------------------------------------------------------
-- Conversion from FAUN
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGFO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGTD.Exp n TFA.Typ <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r)  =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGTD.Exp n TFA.Typ)
         where
  cnv (e , r , v) = do e' :: FAUD.Exp <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r) =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             FAUD.Exp
         where
  cnv (e , _ , vs) = do vs' :: EP.Env TH.Name <- cnv (vs , ())
                        cnv (e , vs')

instance (n ~ Len r) =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FAUN.Exp TH.Name)
         where
  cnv (e , _ , _ ) = pure e

---------------------------------------------------------------------------------
-- Conversion from FAUD
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGFO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGTD.Exp n TFA.Typ <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (n ~ Len r) =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGTD.Exp n TFA.Typ)
         where
  cnv (e , r , _) = do e' :: FGTD.Exp n (Maybe TFA.Typ) <- cnv (e , ET.len r)
                       r' :: ES.Env   n TFA.Typ <- cnv (r , ())
                       cnv (e' , r')

instance (n ~ Len r) =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ r , ES.Env n TH.Name)
             FAUD.Exp
         where
  cnv (e , _ , _ ) = pure e

---------------------------------------------------------------------------------
-- Conversion from FGTD
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGTD.Exp n TFA.Typ , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , r , v) = do e' :: FGFO.Exp r t <- cnv (e , r , v)
                       cnv (e' , r , v)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGTD.Exp n TFA.Typ , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , r , _) = cnv (e , r)

instance (n ~ n' , r ~ r' , n ~ Len r) =>
         Cnv (FGTD.Exp n TFA.Typ , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGTD.Exp n' TFA.Typ)
         where
  cnv (e , _ , _) = return e

---------------------------------------------------------------------------------
-- Conversion from FGFO
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGFO.Exp r t , ET.Env TFG.Typ r , en)
             (FGHO.Exp r' t')
         where
  cnv (e , r , _) = cnv ({- nrm -} e , r)

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGFO.Exp r  t , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGFO.Exp r' t')
         where
  cnv (e , _ , _) = pure e

---------------------------------------------------------------------------------
-- Conversion from FGHO
---------------------------------------------------------------------------------
instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGHO.Exp r  t , ET.Env TFG.Typ r , ES.Env n TH.Name)
             (FGHO.Exp r' t')
         where
  cnv (e , _ , _) = pure e

instance (r ~ r' , t ~ t' , n ~ Len r , HasSin TFG.Typ t') =>
         Cnv (FGHO.Exp r  t , ET.Env TFG.Typ r , ev)
             (FGFO.Exp r' t')
         where
  cnv (e , r , _) = cnv (e , r)

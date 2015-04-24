module QHaskell.Type.Conversion () where

import QHaskell.MyPrelude

import qualified QHaskell.Type.ADT  as TFA
import qualified QHaskell.Type.GADT as TFG
import qualified QHaskell.Type.Herbrand      as HR
import qualified Language.Haskell.TH          as TH

import QHaskell.Conversion

---------------------------------------------------------------------------------
--  Conversion from TFA.Typ
---------------------------------------------------------------------------------

instance Cnv (TFA.Typ) (ExsSin TFG.Typ) where
  cnv TFA.Int         = return (ExsSin TFG.Int)
  cnv TFA.Bol         = return (ExsSin TFG.Bol)
  cnv TFA.Flt         = return (ExsSin TFG.Flt)

  cnv (TFA.Arr ta tr) = do ExsSin ta' <- cnv ta
                           ExsSin tr' <- cnv tr
                           return (ExsSin (TFG.Arr ta' tr'))
  cnv (TFA.Tpl tf ts) = do ExsSin tf' <- cnv tf
                           ExsSin ts' <- cnv ts
                           return (ExsSin (TFG.Tpl tf' ts'))
  cnv (TFA.May t)     = do ExsSin t' <- cnv t
                           return (ExsSin (TFG.May t'))

instance Cnv (TFA.Typ , r) (HR.Typ (HR.EnvFld '[])) where
  cnv (th , r) = let ?r = r in case th of
    TFA.Int       -> pure HR.Int
    TFA.Bol       -> pure HR.Bol
    TFA.Flt       -> pure HR.Flt
    TFA.Arr ta tb -> HR.Arr <$@> ta <*@> tb
    TFA.Tpl tf ts -> HR.Tpl <$@> tf <*@> ts
    TFA.May ta    -> HR.May <$@> ta

instance Cnv (TFA.Typ , r) TH.Type where
  cnv (th , r) = let ?r = r in case th of
    TFA.Int       -> pure (TH.ConT ''Int)
    TFA.Bol       -> pure (TH.ConT ''Bol)
    TFA.Flt       -> pure (TH.ConT ''Flt)
    TFA.Arr ta tb -> do ta' <- cnvImp ta
                        tb' <- cnvImp tb
                        pure (TH.AppT (TH.AppT (TH.ConT ''Arr) ta') tb')
    TFA.Tpl tf ts -> do ta' <- cnvImp tf
                        tb' <- cnvImp ts
                        pure (TH.AppT (TH.AppT (TH.ConT ''Tpl) ta') tb')
    TFA.May ta    -> TH.AppT (TH.ConT ''May) <$@> ta

---------------------------------------------------------------------------------
--  Conversion from TFG.Typ
---------------------------------------------------------------------------------

instance Cnv (TFG.Typ a , r) TFA.Typ where
  cnv (tt , r) = let ?r = r in case tt of
    TFG.Int       -> pure TFA.Int
    TFG.Bol       -> pure TFA.Bol
    TFG.Flt       -> pure TFA.Flt
    TFG.Arr ta tb -> TFA.Arr <$@> ta <*@> tb
    TFG.Tpl tf ts -> TFA.Tpl <$@> tf <*@> ts
    TFG.May ta    -> TFA.May <$@> ta


instance Cnv (TFG.Typ a , r) TH.Type where
  cnv (t , r) = do t' :: TFA.Typ <- cnv (t , r)
                   cnv (t' , r)

---------------------------------------------------------------------------------
--  Conversion from HR.Typ
---------------------------------------------------------------------------------

instance Cnv (HR.Typ (HR.EnvFld '[]) , r) TFA.Typ where
  cnv (th , r) = let ?r = r in case th of
    HR.Int       -> pure TFA.Int
    HR.Bol       -> pure TFA.Bol
    HR.Flt       -> pure TFA.Flt
    HR.Arr ta tb -> TFA.Arr <$@> ta <*@> tb
    HR.Tpl tf ts -> TFA.Tpl <$@> tf <*@> ts
    HR.May t     -> TFA.May <$@> t
    _            -> fail ("Type Error:\n" ++ show th)

instance ts ~ ts' => Cnv (TFG.Typ ts, r) (TFG.Typ ts') where
  cnv = pure . fst

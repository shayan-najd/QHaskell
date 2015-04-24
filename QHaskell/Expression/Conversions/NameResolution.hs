{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QHaskell.Expression.Conversions.NameResolution () where

import QHaskell.MyPrelude
import qualified QHaskell.Expression.ADTUntypedNamed    as FAUN
import qualified QHaskell.Expression.ADTUntypedDebruijn as FAUD
import qualified QHaskell.Environment.Map                        as EM
import qualified QHaskell.Environment.Plain                      as EP
import QHaskell.Variable.Plain
import QHaskell.Conversion
import QHaskell.Variable.Conversion    ()

instance Eq x =>
         Cnv (FAUN.Exp x , EP.Env x) FAUD.Exp where
  cnv (e , r) = cnv (e , zip r [Zro ..])

instance Eq x =>
         Cnv (FAUN.Exp x , EM.Env x Var) FAUD.Exp where
  cnv (ee , r) = let ?r = r in case ee of
    FAUN.Var v -> FAUD.Var <$> EM.get v r
    _          -> $(biGenOverloadedM 'ee ''FAUN.Exp "FAUD" ['FAUN.Var]
     (\ _tt -> [| flip (curry cnv) r |]))


instance Eq x =>
         Cnv ((x , FAUN.Exp x) , EM.Env x Var)
         FAUD.Fun where
  cnv ((x , e) , r) = fmap FAUD.Fun
                      (cnv (e , (x , Zro) : fmap (fmap Suc) r))
{-
instance Cnv (Var, (EP.Env x', EM.Env Var x')) x' where
  cnv (v , r) = EM.get v (snd r)

instance (x ~ x') =>
         Cnv (FAUD.Exp , (EP.Env x , EM.Env Var x)) (FAUN.Exp x') where
  cnv (ee , r) = let ?r = r in case ee of
    FAUD.Var v ->
    _          -> $(biRecAppMQW 'ee ''FAUD.Exp "FAUN" ['FAUD.Var]
                                    (const id))

instance (x ~ x') =>
         Cnv (FAUD.Fun , (EP.Env x , EM.Env Var x)) (x' , FAUN.Exp x')
         where
   cnv (FAUD.Fun e , r) = case r of
     (x : xs , r') -> do e' <- cnv (e ,
                                    (xs , (Zro , x) :
                                    fmap (\(v , n) -> (Suc v , n)) r'))
                         pure (x , e')
     _             -> fail "Bad Name Pool!"
-}

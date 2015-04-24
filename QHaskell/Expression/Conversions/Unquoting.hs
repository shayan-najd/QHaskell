module QHaskell.Expression.Conversions.Unquoting () where

import QHaskell.MyPrelude

import qualified QHaskell.Type.ADT as TA
import qualified QHaskell.Expression.ADTUntypedNamed as AUN
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Desugar as  DTH
import qualified GHC.Types
import Language.Haskell.TH.Instances ()
import QHaskell.Expression.Utils.TemplateHaskell
import QHaskell.Conversion
import QHaskell.Type.Conversion ()

instance Cnv (DTH.MExp , r) (AUN.Exp TH.Name) where
  cnv (ee , r) = let ?r = r in case ee of
    DTH.MLitE l         -> case l of
      TH.IntegerL  i    -> pure (AUN.Int  (fromInteger  i :: Int))
      TH.RationalL i    -> pure (AUN.ConF (fromRational i :: Flt))
      _                 -> fail "Not Supported!"
    DTH.MVarE n
      | n === 'fst          -> do vv1 <- newTHVar
                                  pure (AUN.Abs (vv1 ,
                                        AUN.Fst (AUN.Var vv1)))
      | n === 'snd          -> do vv1 <- newTHVar
                                  pure (AUN.Abs (vv1 ,
                                        AUN.Snd (AUN.Var vv1)))
      | n === 'save         -> do v1 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                       AUN.Mem (AUN.Var v1)))
      | otherwise           -> pure (AUN.Var (stripNameSpace n))
    DTH.MConE n
      | n === 'True         -> pure (AUN.ConB True)
      | n === 'False        -> pure (AUN.ConB False)
      | n === 'Nothing      -> pure AUN.Non
      | n === 'Just         -> do v1 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Som (AUN.Var v1)))
      | n === '(,)          -> do vv1 <- newTHVar
                                  vv2 <- newTHVar
                                  pure (AUN.Abs (vv1 ,
                                        AUN.Abs (vv2 ,
                                        AUN.Tpl (AUN.Var vv1)
                                                (AUN.Var vv1))))
      | otherwise       -> pure (AUN.Var (stripNameSpace n))
    DTH.MAppE (DTH.MAppE (DTH.MConE n) el) l
      | n === '(,)      -> AUN.Tpl  <$@> el <*@> l
    DTH.MAppE (DTH.MConE n) e
      | n === 'Just     -> AUN.Som  <$@> e
      | n === '(,)      -> do v1 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.Tpl e' (AUN.Var v1)))
    DTH.MAppE (DTH.MVarE n) e
      | n === 'fst      -> AUN.Fst  <$@> e
      | n === 'snd      -> AUN.Snd  <$@> e
      | n === 'save     -> AUN.Mem  <$@> e
    DTH.MAppE ef ea     -> AUN.App  <$@> ef <*@> ea
    DTH.MLamE x   eb    -> AUN.Abs  <$@> (x , eb)
    DTH.MSigE e  t      -> AUN.Typ  <$@> t  <*@> e
    DTH.MLetE x el eb   ->
        AUN.Let  <$@> el <*@> (x , eb)
    DTH.MCaseE ec [(DTH.DConPa n [DTH.DVarPa xf , DTH.DVarPa xs],eb)]
         | n === '(,)    -> do v1 <- newTHVar
                               ec' <- cnvImp ec
                               eb' <- cnvImp eb
                               pure (AUN.Let ec' (v1 ,
                                     AUN.Let (AUN.Fst (AUN.Var v1))
                                     (xf ,
                                     AUN.Let (AUN.Snd (AUN.Var v1))
                                     (xs , eb'))))
    DTH.MCaseE ec [(DTH.DConPa nl [] , el),
                   (DTH.DConPa nr [DTH.DVarPa xr] , er)]
        | nl === 'Nothing ,
          nr === 'Just  -> AUN.May <$@> ec <*@> el <*@>
                           (DTH.MLamE xr er)
    DTH.MCaseE ec [(DTH.DConPa nl [DTH.DVarPa xr] , el),
                   (DTH.DConPa nr [] , er)]
        | nr === 'Nothing ,
          nl === 'Just  -> AUN.May <$@> ec <*@> er <*@>
                           (DTH.MLamE xr el)
    DTH.MCaseE _ _      -> fail "case expression form is not supported!"

instance Cnv ((TH.Name , DTH.MExp) , r) (TH.Name , AUN.Exp TH.Name) where
    cnv ((x , e) , r) = let ?r = r
                        in (,) <$> pure (stripNameSpace x) <*@> e

instance Cnv (DTH.DType , r) TA.Typ where
  cnv (th , r) = let ?r = r in case th of
   DTH.DConT n
       | n == ''Word32                     -> pure TA.Int
       | n == ''Int                        -> pure TA.Int
       | n == ''Bol                        -> pure TA.Bol
       | n == ''Bool                       -> pure TA.Bol
       | n == ''Float                      -> pure TA.Flt
       | n == ''Flt                        -> pure TA.Flt
   DTH.DAppT (DTH.DAppT DTH.DArrowT   a) b -> TA.Arr <$@> a <*@> b
   DTH.DAppT (DTH.DAppT (DTH.DConT n) a) b
       | n == ''Arr                        -> TA.Arr <$@> a <*@> b
       | n == ''(,)                        -> TA.Tpl <$@> a <*@> b
       | n == ''Tpl                        -> TA.Tpl <$@> a <*@> b
   DTH.DAppT (DTH.DConT n) a
       | n == ''Maybe                      -> TA.May <$@> a
       | n == ''May                        -> TA.May <$@> a
   _            -> fail ("Syntax not supported:\n" ++ show th)

-- not supported:
--           | DLitT TyLit
--           | DVarT Name
--           | DSigT DType DKind
--           | DForallT [DTyVarBndr] DCxt DType

newTHVar :: NamM ErrM TH.Name
newTHVar = do v1 <- newVar
              return (stripNameSpace (TH.mkName v1))

instance Cnv (TH.Exp , ()) (AUN.Exp TH.Name) where
  cnv (ee , _) =  do
    ee' :: DTH.MExp <- unQM (TH.runQ (DTH.desugar ee))
    cnv (ee' , ())

data QM a = QM {unQM :: StateT GHC.Types.Int ErrM a}

instance Applicative QM where
  pure = QM . pure
  f <*> g = QM (unQM f <*> unQM g)

instance Functor QM where
  fmap f = QM . fmap f . unQM

instance Monad QM where
  return       = QM . return
  (QM m) >>= f = QM (m >>= (unQM .f))


instance TH.Quasi QM where
  qNewName s          = QM (do n <- newVar
                               return (TH.mkName (n++s)))
  qReport b e         = QM (fail (if b
                                  then ("Error: " ++ e)
                                  else ("Warning: " ++ e)))
  qRecover m1 m2      = QM (StateT (\ s -> case runStateT (unQM m1) s of
                                             Lft _ -> runStateT (unQM m2) s
                                             x     -> x))
  qReify  n
    | n === 'False    = QM (return $(do {i <- TH.reify 'False; TH.lift i}))
    | n === 'True     = QM (return $(do {i <- TH.reify 'True; TH.lift i}))
    | n === ''Bool    = QM (return $(do {i <- TH.reify ''Bool; TH.lift i}))
    | n === 'Nothing  = QM (return $(do {i <- TH.reify 'Nothing; TH.lift i}))
    | n === 'Just     = QM (return $(do {i <- TH.reify 'Just; TH.lift i}))
    | n === ''Maybe   = QM (return $(do {i <- TH.reify ''Maybe; TH.lift i}))
    | n === '(,)      = QM (return $(do {i <- TH.reify '(,); TH.lift i}))
    | n === ''(,)     = QM (return $(do {i <- TH.reify ''(,); TH.lift i}))
    | otherwise       = QM (fail ("Not Supported for reification:\n"++ show n))

  qLookupName _ _     = QM (fail "Not Allowed!")
  qReifyInstances _ _ = QM (fail "Not Allowed!")
  qLocation           = QM (fail "Not Allowed!")
  qReifyRoles _       = QM (fail "Not Allowed!")
  qReifyAnnotations _ = QM (fail "Not Allowed!")
  qReifyModule _      = QM (fail "Not Allowed!")
  qAddDependentFile _ = QM (fail "Not Allowed!")
  qAddModFinalizer _  = QM (fail "Not Allowed!")
  qAddTopDecls _      = QM (fail "Not Allowed!")
  qRunIO _            = QM (fail "Not Allowed!")
  qPutQ _             = QM (fail "Not Allowed!")
  qGetQ               = QM (fail "Not Allowed!")

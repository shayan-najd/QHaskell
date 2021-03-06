module QHaskell.ChangeMonad (Chg(..),chg,tilNotChg) where

import QHaskell.MyPrelude

data Chg a = Chg Bool a

deriving instance Functor     Chg
deriving instance Foldable    Chg
deriving instance Traversable Chg

instance Applicative Chg where
  pure      = return
  af <*> aa = do f <- af
                 a <- aa
                 pure (f a)

instance Monad Chg where
  return           = Chg False
  (Chg b x) >>= f = let Chg b' x' = f x
                    in  Chg (b || b') x'

chg :: a -> Chg a
chg = Chg True

tilNotChg :: (a -> Chg a) -> a -> a
tilNotChg f x = case f x of
  Chg False _  -> x
  Chg True  x' -> tilNotChg f x'

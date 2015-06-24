module QHaskell.MyPrelude
       (Num(..),pi,fromIntegral,toRational
       ,floor,log,sqrt,
        Word32,Rational,Integer,
        Float,Char,
        Bool(..),(&&),(||),not,
        IO,print,readFile,writeFile,putStrLn,getArgs,
        fst,snd,
        String,lines,unlines,
        read,
        (.),flip,curry,uncurry,id,const,
        (++),zip,head,dropWhile,iterate,length,zipWith,filter,(!!),
        delete,init,
        ord,nub,
        Maybe(..),fromJust,maybe,
        Enum(..),
        Ord(..),
        Eq(..),
        Show(..),Fractional(..),Integral(..),
        otherwise, impossible , impossibleM,badUse,badTypVal,
        badTypValM,frmRgtZro,
        lookup,
        State,getState,put,modify,runState,execState,evalState,StateT(..)
             ,lift,evalStateT,
        module QHaskell.ErrorMonad,
        module QHaskell.NameMonad,
        module QHaskell.Existential,
        module QHaskell.TraversalGenerator,
        module Data.Monoid,
        module Data.Foldable,
        module Data.Traversable,
        module Data.Functor,
        module Control.Applicative,
        module Control.Monad,
        TVr(..),
        cnd,tpl,save,fix,
        fixM,trace)
       where
import Debug.Trace
import Prelude hiding (Int,mapM,sequence)
import QHaskell.Nat.GADT
import QHaskell.Existential
import Data.Maybe
import QHaskell.ErrorMonad
import QHaskell.NameMonad
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.Functor
import Control.Monad hiding
   (forM,forM_,sequence,sequence_,msum,mapM,mapM_)
import Data.Monoid
import Control.Monad.State hiding (mapM,sequence)
import Data.List
import Data.Word
import Data.Char(ord)
import System.Environment
import QHaskell.TraversalGenerator

frmRgtZro :: NamM ErrM a -> a
frmRgtZro = frmRgt . runNamM

impossible :: a
impossible = error "Impossible!"

impossibleM :: Monad m => m a
impossibleM = fail "Impossible!"

badUse :: String -> a
badUse = error . ("Bad use of " ++)

badTypVal :: a
badTypVal = error "Value of wrong type!"

badTypValM :: Monad m => m a
badTypValM = fail "Value of wrong type!"

getState :: MonadState s m => m s
getState = get

data TVr x = TVar (Nat x)

cnd :: Bool -> s -> s -> s
cnd c t f = if c then t else f

tpl :: a -> b -> (a , b)
tpl = ((,))

save :: a -> a
save = id

fixM :: (a -> ErrM a) -> ErrM a
fixM f = let a = f (frmRgt a) in a

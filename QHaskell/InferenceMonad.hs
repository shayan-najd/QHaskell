module QHaskell.InferenceMonad where

import QHaskell.MyPrelude

import QHaskell.Type.Herbrand
import QHaskell.Nat.ADT

type InfM r a = State (Nat , [HerCon r]) a

newMT :: InfM r (Typ r)
newMT = do (i , x) <- getState
           put (Suc i , x)
           return (Mta i)

addC  :: HerCon r -> InfM r ()
addC c = modify (\ (i , cs) -> (i , c : cs))


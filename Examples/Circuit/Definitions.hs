module Examples.Circuit.Definitions where

import QHaskell

cond :: Bool -> Bool -> Bool -> Bool
cond x y z = if x then y else z

condFloat :: Bool -> Float -> Float -> Float
condFloat x y z = if x then y else z

makeQDSL "BoolLang" ['not,'cond,'condFloat]

type Result = String

compile :: (Show a , Type a) => BoolLang a -> Result
compile = show . evaluate

boolLang :: (Show a , Type a) => Qt a -> ErrM Result
boolLang q = do d <- translate q
                return (compile (normalise d))

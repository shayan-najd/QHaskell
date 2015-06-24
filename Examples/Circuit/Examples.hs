module Examples.Circuit.Examples where

import QHaskell
import Examples.Circuit.Definitions

notQ :: Qt (Bool -> Bool)
notQ = [|| \x -> not x ||]

xorQ :: Qt (Bool -> Bool -> Bool)
xorQ = [|| \ x y -> cond x (not y) y ||]

xorQ' :: BoolLang (Bool -> Bool -> Bool)
xorQ' = $$(qqBoolLang [|| \ x y -> cond  x (not y) y ||])

ex3Q :: Qt Float
ex3Q = [|| let f x = condFloat x (f False) 1
           in  f True ||]

ex1 :: ErrM Result
ex1 = boolLang [|| $$notQ False ||]

ex2 :: ErrM Result
ex2 = boolLang [|| $$xorQ True True ||]

ex2' :: Bool
ex2' = evaluate xorQ' True True

ex3 :: ErrM Result
ex3 = boolLang ex3Q

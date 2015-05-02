module Examples.Lava where

import Prelude hiding (Int,repeat, not)
import qualified Prelude as P

import QHaskell hiding (Int,repeat,not)
import QHaskell.MyPrelude hiding (not)

import QHaskell.Expression.GADTFirstOrder

type Bit = Bool

repeat :: Bool -> Bit
repeat b = b

delay :: Bool -> Bit -> Bit
delay a s = a

not :: Bit -> Bit
not s = P.not s

xor2 :: (Bit,Bit) -> Bit
xor2 (a,b) = if a then not b else b

and2 :: (Bit,Bit) -> Bit
and2 (a,b) = if a then b else False

makeQDSL "Lava" ['delay,'repeat,'not,'xor2,'and2]

runComb :: Qt Bit -> Bool
runComb q = evaluate $ normalise $ frmRgt $ translate q

ex1 = [|| delay True (repeat False) ||]

halfAdd = [|| \(a,b) -> let sum  = xor2 (a,b)
                            cout = and2 (a,b)
                        in (sum,cout) ||]

fullAdd = [|| \(cin,(a,b)) -> let (sum1,car1) = $$halfAdd (a,b)
                                  (sum,car2)  = $$halfAdd (cin,sum1)
                                  cout        = xor2 (car1,car2)
                              in (sum,cout) ||]

{- Not sure how to implement this function
bitAdder = [|| \(cin,bs) -> case bs of
                              [] -> ([],cin)
                              (a:as) -> (sum:sums,cout)
                                where
                                  (sum,car) = $$halfAdd (cin,a)
                                  (sums,cout) = $$bitAdder (car,as) ||]
-}

edge = [|| \inp -> let inp' = delay False inp
                       change = xor2 (inp,inp')
                   in change ||]

toggle = [|| \change -> let out' = delay False out
                            out  = xor2 (change,out')
                        in  out ||]

-- Stealing if-syntax

ex2 = [|| \b c -> if b then not c else False ||]

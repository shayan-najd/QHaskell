module Examples.Lava where

import Prelude hiding (Int,repeat, not)
import qualified Prelude as P

import QHaskell hiding (Int,repeat,not)
import QHaskell.MyPrelude hiding (not)

type Bit = (Int -> Bool)

repeat :: Bool -> Bit
repeat b = \_i -> b

delay :: Bool -> Bit -> Bit
delay a s = \i -> if i == 0 then a else s (i-1)

not :: Bit -> Bit
not s = \i -> P.not (s i)

xor2 :: (Bit,Bit) -> Bit
xor2 (a,b) = \i -> if a i then not b i else b i

and2 :: (Bit,Bit) -> Bit
and2 (a,b) = \i -> if a i then b i else False

makeQDSL "Lava" ['delay,'repeat,'not,'xor2,'and2]

runSeq :: Qt Bit -> [Bool]
runSeq q = let f = evaluate $ normalise $ frmRgt $ translate q
           in map f [0..]

runSeq1 :: Qt (Bit -> Bit) -> [Bool] -> [Bool]
runSeq1 q = let f =  evaluate $ normalise $ frmRgt $ translate q
            in \s -> map (f (\i -> s !! (fromIntegral i))) [0..]

runComb :: Qt Bit -> Bool
runComb q = let f = evaluate $ normalise $ frmRgt $ translate q
            in  f 0

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

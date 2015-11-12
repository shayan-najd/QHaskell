-- A QDSL implementation of Bjesse, Claessen, Sheeran, and Singh's
--   Lava: hardware design in Haskell
--      by Shayan Najd, Josef Svenningsson

-- This implementation focuses on how to reuse evaluation in Lava
module Examples.Lava where

import Prelude hiding (repeat,not,sum,traverse)
import qualified Prelude as P

import Data.Word
import QHaskell hiding (repeat,not,sum)

type Bit = Word32 -> Bool

inv :: Bit -> Bit
inv s = \i -> P.not (s i)

and2 :: (Bit,Bit) -> Bit
and2 (a,b) = \i -> if a i then b i else False

or2 :: (Bit,Bit) -> Bit
or2 (a,b) = \i -> if a i then True else b i

xor2 :: (Bit,Bit) -> Bit
xor2 (a,b) = \i -> if a i then inv b i else b i

delay :: Bool -> Bit -> Bit
delay a s = \i -> if i == 0 then a else s (i-1)

repeat :: Bool -> Bit
repeat b = \_i -> b

makeQDSL "Lava" ['inv,'and2,'or2,'xor2,'delay,'repeat]

runSeq :: Qt Bit -> [Bool]
runSeq q = let f = evaluate $ normalise $ frmRgt $ translate q
           in map f [0..]

runSeq1 :: Qt (Bit -> Bit) -> [Bool] -> [Bool]
runSeq1 q = let f =  evaluate $ normalise $ frmRgt $ translate q
            in \s -> map (f (\i -> s !! (fromIntegral i))) [0..]

runComb :: Qt Bit -> Bool
runComb q = let f = evaluate $ normalise $ frmRgt $ translate q
            in  f 0

-- rep :: Qt (Bit -> Bit)
-- rep = [|| \ b -> let x = delay b x in x ||]

ex1 :: Qt Bit
ex1 = [|| delay True (repeat False) ||]

halfAdd :: Qt ((Bit, Bit) -> (Bit, Bit))
halfAdd = [|| \(a,b) -> let sum  = xor2 (a,b)
                            cout = and2 (a,b)
                        in (sum,cout) ||]

fullAdd :: Qt ((Bit, (Bit, Bit)) -> (Bit, Bit))
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

edge :: Qt (Bit -> Bit)
edge = [|| \inp -> let inp' = delay False inp
                       change = xor2 (inp,inp')
                   in change ||]

toggle :: Qt (Bit -> Bit)
toggle = [|| \change -> let out' = delay False out
                            out  = xor2 (change,out')
                        in  out ||]

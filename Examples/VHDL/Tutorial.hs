module Examples.VHDL.Tutorial where

import Prelude
import QHaskell
import Examples.VHDL.FrontEnd
import Examples.VHDL.Glue

-- Chapter 2

halfAdd = [|| \ (a,b) ->
              let sum   = xor2 a b
                  carry = and2 a b
              in (sum,carry) ||] 

fullAdd = [|| \ (carryIn, (a,b)) ->
              let (sum1,carry1) = $$halfAdd (a,b)
                  (sum ,carry2) = $$halfAdd (carryIn,sum1)
                  carryOut      = xor2 carry2 carry1
              in  (sum, carryOut)
          ||]

-- Exercises 2.5

-- 2.1

swap = [|| \ (a,b) -> (b,a) ||]
copy = [|| \ a -> (a,a) ||]

-- 2.2

sort2 = [|| \(a,b) -> (and2 a b, or2 a b) ||]

-- 2.3

--high = [|| \() -> True ||]
high = [|| True ||]

-- 2.4

mux = [|| \ (sel,(x,y)) -> or2 (and2 (inv sel) x)
                                (and2 sel y)
       ||]
-- 2.5

add3 = [|| \(x,y,z) ->
           let (sum1,c1) = $$halfAdd (x,y)
               (sum,_)   = $$fullAdd (c1,(sum1,z))
           in sum ||]

-- 2.6
-- Skipping for now


-- 3.1

bitAdder (carryIn,[]) = ([],carryIn)
bitAdder (carryIn,a:as) = ([|| fst $$sumcarry ||] : sums,carryOut)
  where
    sumcarry = [|| $$halfAdd ($$carryIn,$$a) ||]
    (sums,carryOut) = bitAdder ([|| snd $$sumcarry ||], as)

adder (carryIn, ([], [])) = ([], carryIn)
adder (carryIn, (a:as, b:bs)) = ([|| fst $$sumcarry ||] : sums, carryOut)
  where
    sumcarry = [|| $$fullAdd ($$carryIn,($$a,$$b)) ||]
    (sums,carryOut) = adder ([|| snd $$sumcarry||],(as,bs))

{-
serial = [|| \circ1 circ2 a ->
             let b = circ1 a
                 c = circ2 b
             in c ||]

serial circ1 circ2 a = c
  where
    b = [|| $$circ1 $$a ||]
    c = [|| $$circ2 $$b ||]
-}
serial circ1 circ2 a = c
  where
    b = circ1 a
    c = circ2 b

row circ (carryIn,[]) = ([], carryIn)
row circ (carryIn, a:as) = ([|| fst $$bAndCarry ||] : bs, carryOut)
  where
    bAndCarry      = [|| $$circ ($$carryIn, $$a) ||]
    (bs, carryOut) = row circ ([|| snd $$bAndCarry ||], as)

-- Identical to Lava
bitAdder' (carry,inps) = row halfAdd (carry,inps)

-- Identical to Lava
adder' (carry, inps) = row fullAdd (carry,inps)

-- Skip section 3.3 because of lack of VHDL generation

-- Also skipping Exercises in 3.4 for now.
{-
par circ1 circ2 (a,b) = (c,d)
  where
    c = [|| $$circ1 $$a ||]
    d = [|| $$circ2 $$b ||]
-}
par circ1 circ2 (a,b) = (c,d)
  where
    c = circ1 a
    d = circ2 b

-- Chapter 4 -- Verification

-- Chapter 5 -- Sequential Circuits

edge = [|| \inp ->
           let inp'   = delay $$low inp
               change = xor2 inp inp'
           in change ||]

toggle = [|| \change ->
             let out' = delay $$low out
                 out  = xor2 change out'
             in out ||]

delayN 0 init inp = inp
delayN n init inp = out
  where
    out  = [|| delay $$init $$rest ||]
    rest = delayN (n-1) init inp

puls n () = out
  where
    out  = delayN (n-1) low last
    last = [|| delay $$high $$out ||]

-- Cannot do 'counter' or 'counterUp' right now because it
-- requires streams of lists

adderSeq = [|| \(a,b) ->
               let carryIn = delay $$low carryOut
                   (sum,carryOut) = $$fullAdd (carryIn,(a,b))
               in sum ||]

rowSeq circ inp = [|| fst $$out ||]
  where
    carryIn = [|| delay $$zero (snd $$out) ||]
    out     = [|| $$circ ($$carryIn,$$inp) ||]

    zero    = low


rowSeqReset circ (reset,inp) = [|| fst $$out ||]
  where
    carryIn = [|| delay $$zero $$carry ||]
    carry   = [|| $$mux ($$reset,(snd $$out,$$zero)) ||]
    out     = [|| $$circ ($$carryIn,$$inp) ||]

    zero    = low

-- Identical to Lava
adderSeqReset = rowSeqReset fullAdd

-- Identical to Lava
rowSeqPeriod n circ inp = out
  where
    reset = puls n ()
    out   = rowSeqReset circ (reset,inp)

-- Identical to Lava
adderSeqPeriod n = rowSeqPeriod n fullAdd

-- Exercises 5.6

-- 5.1

evenSoFar :: Qt (Bool -> Bool)
evenSoFar = [|| \ inp ->
                let mem = delay $$high inp
                    out = xor2 mem inp
                in out
            ||]

-- 5.2

-- flipFlop = 

-- Section 6 -- Sequential Verification

-- Section 7 -- Time Transformations

-- Identical to Lava
adderCom abs = sum
  where
    (sum, carryOut) = row fullAdd (low,abs)

serialToParallel 1 inp = [inp]
serialToParallel n inp = inp : rest
  where
    inp' = [|| delay $$low $$inp ||]
    rest = serialToParallel (n-1) inp'

parallelToSerial (load,[inp]) = out
  where
    out = [|| $$mux ($$load, ($$low,$$inp)) ||]
parallelToSerial (load, inp:inps) = out
  where
    from = parallelToSerial (load, inps)
    prev = [|| delay $$low $$from ||]
    out  = [|| $$mux ($$load, ($$prev,$$inp)) ||]

{- Doesn't type check.
adderSlowedDown n ab = sum
  where
    abs  = serialToParallel n ab
    sums = adderCom abs
    load = puls n ()
    sum  = parallelToSerial (load,sums)
-}

-- Section 7.3 -- Speeding up
-- We have not implemented the 'timeTransform' primitive

{-
adderSpedUp abs = sums
  where
    sums = timeTransform (adderSeqPeriod n) abs
    n    = length abs
-}

-- Chapter 8 -- More connection patterns

(->-) = serial

compose []             inp = inp
compose (circ : circs) inp = out
  where
    x   = [|| $$circ $$inp ||]
    out = compose circs x

{- It doesn't seem possible to splice infix operators

compose1 []             inp = inp
compose1 (circ : circs) inp = out
  where
    rest = compose1 circs inp
    out  = [|| $$circ $$(->-) $$rest ||]
-}
{-
compose2 []           = id
compose2 (circ:circs) = circ ->- compose2 circ
-}

composeN n circ = compose (replicate n circ)

(-|-) = par

halveList inps = (left,right)
  where
    left  = take half inps
    right = drop half inps
    half  = length inps `div` 2

append (a,b) = a ++ b

parl circ1 circ2 =
  halveList ->- (circ1 -|- circ2) ->- append

-- 8.2

-- Identical to Lava
binTree circ [inp] = inp
binTree circ inps  =
  (halveList ->- (binTree circ -|- binTree circ) ->- circ) inps

-- Identical to Lava
binAdder (as, bs) = cs ++ [carryOut]
  where
    (cs, carryOut) = adder (low, (as, bs))

addTree = binTree binAdder

-- We don't have integers so we can't define this function
-- wrapAddTree

-- 8.3 Butterfly circuits

two circ = parl circ circ

test1 = two reverse [1..16]
test2 = two (two reverse) [1..16]

ilv circ = unriffle ->- two circ ->- riffle

-- Exercises 8.5

-- 8.6

riffle as = uncurry interleave (halveList as)

interleave [] js = js
interleave (i:is) js = i : interleave js is

-- 8.7

unriffle as = even as ++ odd as
  where
    even (a:as) = a : odd as
    even [] = []
    odd  (_:as) = even as
    odd  [] = []


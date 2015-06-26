module Examples.VHDL.Tutorial where

import Prelude
import QHaskell
import Examples.VHDL.FrontEnd
import Examples.VHDL.Glue

----------------------------------------
-- Chapter 2

halfAdd = [|| \ (a,b) ->
              let sum   = xor2 a b
                  carry = and2 a b
              in (sum,carry) ||] 

-- Alternative definition of 'halfAdd' which moves the handling of
-- tuples to the meta language. This allows for a definition of
-- 'bitAdder' which doesn't duplicate the adder circuit. However,
-- the inputs 'a' and 'b' are now being duplicated instead.

halfAdd_ (a,b) = (sum,carry)
  where
    sum   = [|| xor2 $$a $$b ||]
    carry = [|| and2 $$a $$b ||]

fullAdd = [|| \ (carryIn, (a,b)) ->
              let (sum1,carry1) = $$halfAdd (a,b)
                  (sum ,carry2) = $$halfAdd (carryIn,sum1)
                  carryOut      = xor2 carry2 carry1
              in  (sum, carryOut)
          ||]

----------------------------------------
-- Exercises 2.5

-- 2.1

swap = [|| \ (a,b) -> (b,a) ||]
copy = [|| \ a -> (a,a) ||]

-- 2.2

sort2 = [|| \(a,b) -> (and2 a b, or2 a b) ||]
twoBitSort (a,b) = ([|| and2 $$a $$b ||],[|| or2 $$a $$b ||])

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

----------------------------------------
-- 3.1

-- WARNING: 'sumcarry' is duplicated in both 'bitAdder' and 'adder'
-- below. I don't know how to avoid this problem.
-- 'bitAdder_' doesn't suffer from the same problem but instead
-- duplicates all the carry signals. Both versions yield exponential
-- circuits.

bitAdder (carryIn,[]) = ([],carryIn)
bitAdder (carryIn,a:as) = ([|| fst $$sumcarry ||] : sums,carryOut)
  where
    sumcarry = [|| $$halfAdd ($$carryIn,$$a) ||]
    (sums,carryOut) = bitAdder ([|| snd $$sumcarry ||], as)

bitAdder_ (carryIn,[]) = ([],carryIn)
bitAdder_ (carryIn,a:as) = (sum:sums,carryOut)
  where
    (sum ,carry   ) = halfAdd_  (carryIn, a)
    (sums,carryOut) = bitAdder_ (carry,  as)

adder (carryIn, ([], [])) = ([], carryIn)
adder (carryIn, (a:as, b:bs)) = ([|| fst $$sumcarry ||] : sums, carryOut)
  where
    sumcarry = [|| $$fullAdd ($$carryIn,($$a,$$b)) ||]
    (sums,carryOut) = adder ([|| snd $$sumcarry||],(as,bs))

{- Various variations of 'serial':

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

-- WARNING: More duplication going on here

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

----------------------------------------
-- Exercises 3.4

-- 3.2

adder2 (as, bs) = s
  where
    (s, arryOut) = adder (low, (as, bs))

-- 3.5

zipp ([], []) = []
zipp (a:as, b:bs) = (a,b) : rest
  where
    rest = zipp (as, bs)

unzipp [] = ([],[])
unzipp ((a,b):abs) = (a:as, b:bs)
  where
    (as, bs) = unzipp abs

-- 3.6

pair (x:y:xs) = (x,y) : pair xs
pair xs = []

unpair ((x,y):xys) = x : y : unpair xys
unpair [] = []

-- 3.7

par circ1 circ2 (a,b) = (c,d)
  where
    c = circ1 a
    d = circ2 b

{- Alternative version where circuits are using quoted application
   but it does not work well with other combinators.

par circ1 circ2 (a,b) = (c,d)
  where
    c = [|| $$circ1 $$a ||]
    d = [|| $$circ2 $$b ||]
-}

-- 3.9

column circ ([],   carryIn) = (carryIn,  [])
column circ (a:as, carryIn) = (carryOut, b:bs)
  where
    (carry, b)     = circ (a, carryIn)
    (carryOut, bs) = column circ (as, carry)

-- 3.10

-- Doesn't type check right now
-- grid circ = row (column circ)

-- Chapter 4 -- Verification

----------------------------------------
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

-- WARNING: Duplication once again!
-- Both 'rowSeq' and 'rowSeqReset'.

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

----------------------------------------
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

----------------------------------------
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

----------------------------------------
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

----------------------------------------
-- 8.3 Butterfly circuits

two circ = parl circ circ

test1 = two reverse [1..16]
test2 = two (two reverse) [1..16]

ilv circ = unriffle ->- two circ ->- riffle

twoN 0 circ = circ
twoN n circ = two (twoN (n-1) circ)

ilvN 0 circ = circ
ilvN n circ = ilv (ilvN (n-1) circ)

iter 0 comb circ = circ
iter n comb circ = comb (iter (n-1) comb circ)

bfly 0 circ = id
bfly n circ = ilv (bfly (n-1) circ) ->- twoN (n-1) circ

bfly1 0 circ = id
bfly1 n circ = ilvN (n-1) circ ->- two (bfly1 (n-1) circ)

bfly2 0 circ = id
bfly2 n circ = ilvN (n-1) circ ->- bfly2 (n-1) (two circ)

bfly3 0 circ = id
bfly3 n circ = bfly3 (n-1) (ilv circ) ->- twoN (n-1) circ

listTree circ [inp] = [inp]
listTree circ inps = (two (listTree circ) ->- circ) inps

ilvTree circ [inp] = [inp]
ilvTree circ inps  = (ilv (ilvTree circ) ->- circ) inps

pmap circ = pair ->- map circ ->- unpair

swapl [a,b] = [b,a]

s2 [a,b,c,d] = [a,b,c,d]

----------------------------------------
-- Batcher's Bitonic Merger 8.4
{-
compUp   [x,y] = [imin (x,y), imax (x,y)]
compDown [x,y] = [imax (x,y), imin (x,y)]
-}
sorter 0 comp [inp] = [inp]
sorter n comp inps = outs
  where
    sortL = sorter (n-1) comp
    sortR = sorter (n-1) (comp ->- swapl) -- reversed omparator
    merger = bfly n comp -- bitonic merger
    outs = (parl sortL sortR ->- merger) inps

twoBitSortl [a,b] = [min, max]
  where
    (min, max) = twoBitSort (a, b)

----------------------------------------
-- Exercises 8.5

-- 8.6
{-
riffle as = uncurry interleave (halveList as)

interleave [] js = js
interleave (i:is) js = i : interleave js is
-}

riffle = halveList ->- zipp ->- unpair

-- 8.7

{-
unriffle as = even as ++ odd as
  where
    even (a:as) = a : odd as
    even [] = []
    odd  (_:as) = even as
    odd  [] = []
-}

unriffle = pair ->- unzipp ->- append

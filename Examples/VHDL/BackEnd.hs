module Examples.VHDL.BackEnd where

import Prelude

import Data.Word
import Data.List (intersperse,delete)
import Control.Applicative
import Control.Monad.State

import QHaskell.ChangeMonad

-----------------------------------------------------------------------
-- AST
-----------------------------------------------------------------------

-- | MiniLava's abstract syntax
data Exp
  = Bool  Bool
  | VarB  String
  | Fix   String Exp
  | Let   String Exp Exp
  | Fst   Exp
  | Snd   Exp
  | Pair  Exp Exp
  | Inv   Exp
  | And   Exp Exp
  | Or    Exp Exp
  | Xor   Exp Exp
  | Delay Exp Exp

deriving instance Show Exp
deriving instance Eq   Exp

-----------------------------------------------------------------------
-- Functions Related to Bindings
-----------------------------------------------------------------------

-- | returns a list of all variables in a Exp
vars :: Exp -> [String]
vars l = case l of
  Bool _    -> []
  VarB  x   -> [x]
  Inv   m   -> vars1 m
  Fst   m   -> vars1 m
  Snd   m   -> vars1 m
  And   m n -> vars2 m n
  Or    m n -> vars2 m n
  Xor   m n -> vars2 m n
  Delay m n -> vars2 m n
  Pair  m n -> vars2 m n
  Fix   _ _ -> error "Bad use of vars"
  Let _ _ _ -> error "Bad use of vars"
 where
   vars1 m   = vars m
   vars2 m n = vars m ++ vars n

-- | renames occurences of the given variable
rename :: String -> String -> Exp -> Exp
rename x y l = case l of
  Bool _       -> l
  VarB z
   | z == x    -> VarB y
   | otherwise -> l
  Inv   m      -> rename1 Inv   m
  Fst   m      -> rename1 Fst   m
  Snd   m      -> rename1 Snd   m
  And   m n    -> rename2 And   m n
  Or    m n    -> rename2 Or    m n
  Xor   m n    -> rename2 Xor   m n
  Delay m n    -> rename2 Delay m n
  Pair  m n    -> rename2 Pair  m n
  Fix   _ _    -> error "Bad use of rename"
  Let _ _ _    -> error "Bad use of rename"
 where
   rename1 c m   =  c (rename x y m)
   rename2 c m n =  c (rename x y m) (rename x y n)

-----------------------------------------------------------------------
-- Translation to Netlist
-----------------------------------------------------------------------

-- | type of directed graphs formed by annotating every subterm by a
-- unique key, and replacing their by a varaible pointing to their
-- corresponding unique key
type Graph = [(String , Exp)]

-- | type of netlists (rooted graphs)
type Netlist = (Graph , [String])

-- | generates a netlist from a list of circuit elements (Lava program)
netlist :: [Exp] -> Netlist
netlist ss = runNameMonad
             (do vs <- sequence [ fmap (\ i -> "w" ++ show i) newVar
                                | _ <- ss]
                 ls <- sequence [ netlistOneM v s
                                | (v , s) <- zip vs ss]
                 return (concat ls , vs))

-- | a single monadic step for generating netlist
netlistOneM :: String -> Exp -> NameMonad Graph
netlistOneM y l = case l of
  Bool _    -> return ([(y, l)])
  VarB _    -> return ([(y, l)])
  Fix  f m  -> do lm <- netlistOneM f m
                  return ((y , VarB f) : lm)
  Let x m n -> do lm <- netlistOneM x m
                  ln <- netlistOneM y n
                  return (lm ++ ln)
  Inv   m   -> netlistOneM1 Inv   m
  Fst   m   -> netlistOneM1 Fst   m
  Snd   m   -> netlistOneM1 Snd   m
  And   m n -> netlistOneM2 And   m n
  Or    m n -> netlistOneM2 Or    m n
  Xor   m n -> netlistOneM2 Xor   m n
  Delay m n -> netlistOneM2 Delay m n
  Pair  m n -> netlistOneM2 Pair  m n
 where
  netlistOneM1 c m   = do xm <- newVar
                          lm <- netlistOneM (w xm) m
                          return ((y , c (VarB (w xm))) : lm)
  netlistOneM2 c m n = do xm <- newVar
                          xn <- newVar
                          lm <- netlistOneM (w xm) m
                          ln <- netlistOneM (w xn) n
                          return ((y , c (VarB (w xm)) (VarB (w xn))) :
                                       lm ++ ln)
  w i = "w" ++ show i

-----------------------------------------------------------------------
-- Optimisation of Netlists
-----------------------------------------------------------------------

-- | eliminating identity nodes
idElim :: Graph -> Netlist -> Chg Netlist
idElim []                   nl        = pure nl
idElim (p@(x,VarB y) : _)   (xs , rt) = chg (onFst (delete p)
                                             ( [ onSnd (rename x y) n
                                               | n <- xs]
                                             , update x y rt))
idElim (_            : xys) nl        = idElim xys nl

-- | eliminating pairs
pairElim :: Graph -> Netlist -> Chg Netlist
pairElim []                        nl    = pure nl
pairElim ((x , Fst (VarB y)) : _)  nl
  | Just (Pair m _) <- lookup y (fst nl) =
      chg (onFst (updateMap x m) nl)
pairElim ((x , Snd (VarB y)) : _)  nl
  | Just (Pair _ n) <- lookup y (fst nl) =
      chg (onFst (updateMap x n) nl)
pairElim (_ : ys)                  nl    = pairElim ys nl

-- | eliminating dead code
deadCodeElim :: Graph -> Netlist -> Chg Netlist
deadCodeElim  []              nl           = pure nl
deadCodeElim  ((x , s) : xys) nl@(xs , rt)
  | x `notElem`
    (rt ++ concatMap vars (fmap snd xs))   =
      chg (onFst (delete (x , s)) nl)
  | otherwise                              = deadCodeElim xys nl

-- | the optimiser, which recursively applies rules for
--   (a) elimination of identity nodes,
--   (b) elimination of pairs, and
--   (c) elimination of dead-code.
optimise :: Netlist -> Netlist
optimise = tilNotChg
           (\ nl0 -> do nl1 <- idElim   (fst nl0) nl0
                        nl2 <- pairElim (fst nl1) nl1
                        deadCodeElim (fst nl2) nl2)

-----------------------------------------------------------------------
-- VHDL Generator
-----------------------------------------------------------------------

-- | generates VHDL code, provided
-- (a) a name for the circuit,
-- (b) names of the input wires, and
-- (c) a list of circuit elements (Lava program).
vhdl :: String -> [String] -> [Exp] -> String
vhdl name inp out =
  let (g , outvs) = optimise $ netlist out
      out'    = ["outp_" ++ show i | i <- [0..(length out - 1)]]
  in  (unlines $
       [ "-- Generated by Lava 2000"
       , ""
       , "use work.all;"
       , ""
       , "entity"
       , "  " ++ name
       , "is"
       , "port"
       , "  -- clock"
       , "  ( clk : in bit"
       , ""
       , "  -- inputs"
       ]
       ++
       [ "  ; " ++ v ++ " : in bit" | v <- inp]
       ++
       [ ""
       , "  -- outputs"
       ]
       ++
       [ "  ; " ++ v ++ " : out bit" | v <- out']
       ++
       [ "  );"
       , "end entity " ++ name ++ ";"
       , ""
       , "architecture"
       , "  structural"
       , "of"
       , "  " ++ name
       , "is"
       ]
       ++
       [ "  signal " ++ x ++ " : bit;" | (x , _) <- g]
       ++
       [ "begin"]
       ++
       [ define x s | (x , s) <- g]
       ++
       [ ""
       , "  -- naming outputs"]
       ++
       [ define v' (VarB v)
       | (v,v') <- outvs `zip` out']
       ++
       [ "end structural;"])

-- | generates VHDL code for given node
define :: String -> Exp -> String
define v s = case s of
  Bool True  -> port "vdd"   []
  Bool False -> port "gnd"   []
  Inv x      -> port "inv"   [x]
  And x y    -> port "and2"  [x,y]
  Or  x y    -> port "or2"   [x,y]
  Xor x y    -> port "xor2"  [x,y]
  VarB s'    -> port "id"    [VarB s']
  Delay x y  -> port "delay" [x, y]
  _          -> error ("Shouldn't have come here!\n" ++ show s)
 where
      port name' argss =
        let args = [vv | VarB vv <- argss] in
            "  "
         ++ make 9 ("c_" ++ v)
         ++ " : entity "
         ++ make 5 name'
         ++ " port map ("
         ++ concat (intersperse ", " ("clk" : args ++ [v]))
         ++ ");"
      make n ss = take (n `max` length ss) (ss ++ repeat ' ')

-----------------------------------------------------------------------
-- Helpter Functions
-----------------------------------------------------------------------

-- | type of a monad for generating fresh names
type NameMonad a = State Word32 a

-- | generates a fresh name in the name monad
newVar :: NameMonad Word32
newVar = do n <- get
            let n' = n + 1
            put n'
            return n'

-- | runs the given name monad
runNameMonad :: NameMonad a -> a
runNameMonad = flip evalState 0

-- | updates corresponding value of a given key in a map
updateMap :: Eq a => a -> b -> [(a , b)] -> [(a , b)]
updateMap x y xys = [ (a , if x == a
                           then y
                           else b)
                    | (a , b) <- xys]

-- | updates an element in the list
update :: Eq a => a -> a -> [a] -> [a]
update x y as = [ if x == a
                  then y
                  else a
                | a <- as]

-- | updates the first element of the list
onFst :: (a -> c) -> (a , b) -> (c , b)
onFst f (m , n) = (f m , n)

-- | updates the second element of the list
onSnd :: (b -> c) -> (a , b) -> (a , c)
onSnd f (m , n) = (m , f n)

-----------------------------------------------------------------------
-- Examples and Dumps
-----------------------------------------------------------------------

-- toggle circuit, implemented using directly the data constructors
ex1 :: Exp
ex1 = Snd (Fix "x" (Pair (Delay (Bool False) (Snd (VarB "x")))
                    (Xor (VarB "input") (Fst (VarB "x")))))

-- optimised netlist of toggle circuit
ex2 :: Netlist
ex2 =  optimise (netlist [ex1])

-- normal hypothetical program close to ex1,
-- does not work unless original Lava method (Observable Sharing)
-- is employed
toggle :: Exp -> Exp
toggle input = let w3 = Delay (Bool False) w7
                   w7 = Xor input w3
               in w7

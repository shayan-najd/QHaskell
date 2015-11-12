     QuickDSL, a Tool for Implementing Quoted DSLs in Haskell
                     Shayan Najd - Sep. 2014
=====================================================================

Abstract
--------
In this note, I present QuickDSL, a tool for implementing Quoted DSLs
in Haskell neatly and quickly.

Introduction (as a user manual)
-------------------------------

Architecture
------------
QuickDSL has a very simple architecture. It exports three functions:

< tran :: Type a => TypeEnv g -> Qt a -> ErrM (Dp g a)
< norm :: Type a => Dp g a -> Dp g a
< eval :: Type a => EvalEnv g -> Dp g a -> a

where |TypeEnv g| is a datatype containing a list of name, and
corresponing simple type, of built-in language constructs, used for
typechecking; |EvalEnv g| is a datatype containing a list of haskell
functions, corresponding to each built-in language constructs, used
for evaluation; |Qt a| represents quoted expressions of type |a|
in typed Template Haskell; and |Dp g a| is type of the final
representation of terms (of type |a|, under the typing environment
|g|). The typeclass |Type a| provides a term-level representation of
type |a|.

The |tran| function translates the typed quotation, under the
typing environment provided, to a representation which carries all the
typing information.

QuickDSL also provides a library to convert from the final
representation mentioned above to some of the well-known alternative
represenations (e.g. untyped ADT representation).

The |norm| function normalises the input to a normal form which
respects the proper sub-formula property.

The |eval| function evaluates the input term to a value in Haskell.

QuickDSL also provides a library of some well-known transformations on
the final representation (e.g. common sub-expression elimination).

Finally, QuickDSL provides the following function to automatically
generate typing environment, evaluation environment, and the necessary
plumbing:

< makeQDSL :: String -> [TH.Name] -> TH.Q [TH.Dec]

It takes name of the DSL, and a list of names of the Haskell functions
whose types are used for typing primitives with the same name and whose
bodies are used for automatically evaluating the primitives of the same
name. It produces functions specialised to work only with
the generated DSL:

< type Types = ... -- signature (a list of types) of built-in constructs
< type NameOfDSL a = Dp Types a
<
< typeEnv :: TypeEnv Types
< typeEnv = ...
<
< evalEnv :: EvalEnv Types
< evalEnv = ...
<
< translate :: Type a => Qt a -> ErrM (NameOfDSL a)
< translate = tran typeEnv
<
< normalise :: Type a => NameOfDSL a -> NameOfDSL a
< normalise = norm
<
< evaluate :: Type a => NameOfDSL a -> a
< evaluate = eval evalEnv


Getting Started
---------------
In order to get started, the user (i.e. the QDSL developer, as opposed
to the end-user) has to follow the following steps.

Step 1:
The user downloads QHaskell from GitHub, and installs it by
running the following command in the downloaded folder:

< $> cabal install

Step 2:
The user writes a module containing one function per built-in
construct with explicit type annotation. This module is to be used to
bypass Haskell's limitaiton of lacking proper support for open
quotations.

> module Examples.MathLang where
>
> import QHaskell hiding (div)
>
> add :: Float -> Float -> Float
> add = (+)
>
> sub :: Float -> Float -> Float
> sub = (-)
>
> mul :: Float -> Float -> Float
> mul = (*)
>
> div :: Float -> Float -> Float
> div = (/)


The names exported by the module (with their corresponding types) are
used to form the typeing environment of built-in constructs needed for
translation. The user uses |makeQDSL| to generate the necessary code and
plumbing.

> makeQDSL "MathLang" ['add,'sub,'mul,'div]

Above generates the following:

< type Types = [Float -> Float -> Float,
<               Float -> Float -> Float]
<
< type MathLang a = Dp Types a
<
< typeEnv :: TypeEnv Types
< typeEnv = 'add <:> 'sub <:> nil
<
< evalEnv :: EvalEnv Types
< evalEnv = add <+> sub <+> nil
<
< translate :: Type a => Qt a -> ErrM (MathLang a)
< translate = tran typeEnv
<
< normalise :: Type a => MathLang a -> MathLang a
< normalise = norm
<
< evaluate :: Type a => MathLang a -> a
< evaluate = eval evalEnv

Step 3:
The user writes a back-end to translate the normalised final
representation to the desired intermediate language, or to directly
use it for building a compiler.

> type Result = String
>
> -- an very simplified compiler that takes the expression
> -- evaluates it and prints it
> compile :: (Show a , Type a) => MathLang a -> Result
> compile = show . evaluate
>
> mathLang :: (Show a , Type a) => Qt a -> ErrM Result
> mathLang q = do d <- translate q
>                 return (compile (normalise d))

Step 4:
The domain experts can use quotations to program in the DSL defined above.

> fourtyTwo = mathLang [|| add 2.0 (sub 50.0 10.0) ||]
> double = [|| \x -> add x x ||]
> fourtyTwo' = mathLang [|| $$double 21 ||]

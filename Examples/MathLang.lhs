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
for evaluation, |Qt a| represents quoted expressions of type |a|
in typed Template Haskell; and |Dp g a| is type of the final
representation of terms (of type |a|, under the typing environment
|g|). The typeclass |Type a| provides a term-level representation of
type |a|.

The |tran| function translates the typed quotation, under the
typing environment provided, to a representation which carries all the
typing information.

QuickDSL also provides a library to convert from the final
representation mentioned above to some of the well-known alternative
represenations.

The |norm| function normalises the input to a normal form which
respects the proper sub-formula property.

The |eval| function evaluates the input term to a value in Haskell.

QuickDSL also provides a library of some well-known transformations on
the final representation.

Getting Started
---------------
In order to get started, the user (i.e. the QDSL developer, as opposed
to the end-user) has to follow the following steps.

Step 1:
The user downloads Haskell-QDSL from GitHub, and installs it by
running the following command in the downloaded folder:

< $> cabal install

Step 2:
The user writes a module containing one function per built-in
construct with explicit type annotation. This module is to be used to
bypass Haskell's limitaiton of lacking proper support for open
quotations. QuickDSL can provide tools to automate part of this
procedure.

> module Examples.MathLang where
>
> import QHaskell
>
> add :: Float -> Float -> Float
> add = (+)
>
> sub :: Float -> Float -> Float
> sub = (-)

The names exported by the module (with their corresponding types) are
used to form the typeing environment of built-in constructs needed for
translation.

> type Types = [Float -> Float -> Float,
>               Float -> Float -> Float]
>
> typeEnv :: TypeEnv Types
> typeEnv = 'add <:> 'sub <:> nil
>
> evalEnv :: EvalEnv Types
> evalEnv = add <+> sub <+> nil
>
> type MathLang a = Dp Types a


Step 3:
The user writes a back-end to translate the normalised final
representation to the desired intermediate language, or to directly
use it for building a compiler.

> type Result = String
>
> -- an very simplified compiler that takes the expression
> -- evaluates it and prints it
> compile :: (Show a , Type a) => MathLang a -> Result
> compile = show . eval evalEnv
>
> mathLang :: (Show a , Type a) => Qt a -> ErrM Result
> mathLang q = do d <- tran typeEnv q
>                 return (compile (norm d))

Step 4:
The domain experts can use quotations to program in the DSL defined above.

> fourtyTwo = mathLang [|| add 2.0 (sub 50.0 10.0) ||]
> double = [|| \x -> add x x ||]
> fourtyTwo' = mathLang [|| $$double 21 ||]

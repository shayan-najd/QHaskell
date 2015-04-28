-- A QDSL implementation of Carlson, Hudak, and Jones'
--   Geometric Region Servers,
--      by Josef Svenningsson
module Examples.Region where

import QHaskell

type Point = (Float,Float)
type Region = Point -> Bool
type Radius = Float

circle   :: Radius -> Region
circle r = \p -> magnitude p <= r

outside  :: Region -> Region
outside r = \p -> not (r p)

intersection :: Region -> Region -> Region
intersection r1 r2 = \p -> r1 p && r2 p

union :: Region -> Region -> Region
union r1 r2 = \p -> r1 p || r2 p

magnitude :: Point -> Float
magnitude (x,y) = sqrt (x ** 2 + y ** 2)

makeQDSL "RegionLang" ['circle,'outside,'intersection,'union,'magnitude]

{-
\begin{Code generated automatically by a Template Haskell function}

type Types = [Radius -> Region
             ,Region -> Region
             ,Region -> Region -> Region
             ,Region -> Region -> Region
             ,Point  -> Float]

typeEnv :: TypeEnv Types
typeEnv = 'circle <:> 'outside <:> 'intersection <:> 'union <:>
           'magnitude <:> nil

evalEnv :: EvalEnv Types
evalEnv = circle <+> outside <+> intersection <+> union <+>
          magnitude <+> nil

translate :: Type a => Qt a -> ErrM (RegionLang a)
translate = tran typeEnv

evaluate :: Type a => RegionLang a -> a
evaluate = eval evalEnv

normalise :: Type a => RegionLang a -> RegionLang a
normalise = norm

type RegionLang a = Dp Types a

qqRegionLang :: Type a => Qt a -> Qt (RegionLang a)
qqRegionLang = tranQ typeEnv

\end{Code generated automatically by a Template Haskell function}
-}

type Result = Region

compile :: Type a => RegionLang a -> a
compile = evaluate

regionLang :: Qt Region -> ErrM Region
regionLang q = do d <- translate q
                  return (compile (normalise d))

inRegion :: Qt Region -> Point -> ErrM Bool
inRegion q p = do f <- regionLang q
                  return (f p)

{-
inRegion :: Qt (Point  -> Region -> Bool)
inRegion = [|| \ p r -> r p ||]
-}

test :: ErrM Bool
test = inRegion [|| outside (circle 3) ||] (5,5)

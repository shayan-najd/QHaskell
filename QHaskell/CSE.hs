{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-max-relevant-binds #-}
module QHaskell.CSE where

import QHaskell.Expression.GADTFirstOrder
import QHaskell.Expression.Utils.Equality.GADTFirstOrder
import QHaskell.Expression.Utils.GADTFirstOrder(prdAllM,sucAll,isVal)
import QHaskell.Expression.Utils.Common

import QHaskell.MyPrelude hiding (foldl)

import qualified QHaskell.Type.GADT as TFG
import QHaskell.ChangeMonad
import QHaskell.Singleton
import QHaskell.Variable.Typed

type family Env a :: [*]
type instance Env (Exp r t) = r

cse :: HasSin TFG.Typ t => Exp r t -> Exp r t
cse = tilNotChg cseOne

cseOne :: forall g a. HasSin TFG.Typ a => Exp g a -> Chg (Exp g a)
cseOne ee = let t = sin :: TFG.Typ a in case ee of
  ConI i                    -> pure (ConI i)
  ConB b                    -> pure (ConB b)
  ConF f                    -> pure (ConF f)
  Var v                     -> pure (Var v)
  Abs eb                    -> case TFG.getPrfHasSinArr t of
    (PrfHasSin , PrfHasSin) -> Abs <$> cseOne eb
  App ef ea                 -> cseOne2 App ef ea
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> cseOne2 Tpl ef es
  Fst e                     -> Fst <$> cseOne e
  Snd e                     -> Snd <$> cseOne e
  Let el eb                 -> cseOne2F Let el eb
  Tag x e                   -> Tag x <$> cseOne e
  Non                       -> pure Non
  Som eb                    -> case TFG.getPrfHasSinMay t of
    PrfHasSin               -> Som <$> cseOne eb
  May l m n                 -> cseOne3 May l m n
  Int  i                    -> pure (Int i)
  Mem  m                    -> Mem <$> cseOne m

remTag :: forall r t. Exp r t -> Exp r t
remTag ee = case ee of
  Tag _  e  -> remTag e
  _         -> $(genOverloaded 'ee ''Exp  ['Tag]
   (\ tt -> if
    | matchQ tt [t| Exp t t |]            -> [| remTag |]
    | otherwise                           -> [| id |]))

remTheTag :: forall r t. String -> Exp r t -> Exp r t
remTheTag x ee = case ee of
  Tag y e
      | x == y    -> e
      | otherwise -> Tag y (remTheTag x e)
  _               -> $(genOverloaded 'ee ''Exp  ['Tag]
   (\ tt -> if
    | matchQ tt [t| Exp t t |]            -> [| remTheTag  x |]
    | otherwise                           -> [| id |]))
{-
cseOne3 :: (HasSin TFG.Typ t , HasSin TFG.Typ a , HasSin TFG.Typ b , HasSin TFG.Typ c) =>
           (Exp g a -> Exp g b -> Exp g c -> Exp g t) ->
           Exp g a -> Exp g b -> Exp g c -> Chg (Exp g t)
cseOne3 k l m n  = case findTags l of
    tgs@(_:_) -> let f tss = case tss of
                       [] ->  k <$> cseOne l <*> cseOne m <*> cseOne n
                       ((x , Exs1 ex tx) : ts) -> case getPrfHasSin tx of
                                     PrfHasSin -> if hasTag x ex m || hasTag x ex n
                                                  then chg (Let ex (absTag (Var Zro) x (sucAll ex) (sucAll (k l m n))))
                                                  else f ts
                 in f tgs
    []        -> case findTags m of
      tgs@(_:_) -> let f tss = case tss of
                        [] ->  k <$> cseOne l <*> cseOne m <*> cseOne n
                        ((x , Exs1 ex tx) : ts) -> case getPrfHasSin tx of
                                      PrfHasSin -> if hasTag x ex n
                                                   then chg (Let ex (absTag (Var Zro) x (sucAll ex) (sucAll (k l m n))))
                                                   else f ts
                   in f tgs
      [] -> k <$> cseOne l <*> cseOne m <*> cseOne n

cseOne2 :: (HasSin TFG.Typ a, HasSin TFG.Typ b, HasSin TFG.Typ c) =>
           (Exp g a -> Exp g b -> Exp g c) ->
            Exp g a -> Exp g b -> Chg (Exp g c)
cseOne2 k m n = case findTags m of
    tgs@(_:_) -> let f tss = case tss of
                       [] ->  k <$> cseOne m <*> cseOne n
                       ((x , Exs1 ex tx) : ts) -> case getPrfHasSin tx of
                                     PrfHasSin -> if hasTag x ex n
                                                  then chg (Let ex (absTag (Var Zro) x (sucAll ex) (sucAll (k m n))))
                                                  else f ts
                 in f tgs
    [] -> k <$> cseOne m <*> cseOne n

cseOne2F :: (HasSin TFG.Typ a, HasSin TFG.Typ b, HasSin TFG.Typ c) =>
           (Exp g a -> Exp (t ': g) b -> Exp g c) ->
            Exp g a -> Exp (t ': g) b -> Chg (Exp g c)
cseOne2F k m n = case findTags m of
    tgs@(_:_) -> let f tss = case tss of
                       [] ->  k <$> cseOne m <*> cseOne n
                       ((x , Exs1 ex tx) : ts) -> case getPrfHasSin tx of
                                     PrfHasSin -> if hasTag x (sucAll ex) n
                                                  then chg (Let ex (absTag (Var Zro) x (sucAll ex) (sucAll (k m n))))
                                                  else f ts
                 in f tgs
    [] -> k <$> cseOne m <*> cseOne n

absTag :: forall t t' g.
          (HasSin TFG.Typ t , HasSin TFG.Typ t') =>
          Exp g t' -> String -> Exp g t' -> Exp g t -> Exp g t
absTag xx s ex ee = let t = sin :: TFG.Typ t in case ee of
  Tag x e
   | s == x    -> case eqlSin (sinTyp xx) t of
     Rgt Rfl   -> if eql e ex
                  then xx
                  else Tag x (absTag xx s ex e)
     _         -> impossible
   | otherwise -> Tag x (absTag xx s ex e)
  _            -> $(genOverloadedW 'ee ''Exp  ['Tag] (trvWrp 't)
   (\ tt -> if
     | matchQ tt [t| Exp (t ': t) t |]     -> [| absTag (sucAll xx) s (sucAll ex) |]
     | matchQ tt [t| Exp t t |]            -> [| absTag xx s ex |]
     | otherwise                           -> [| id |]))

hasTag :: (HasSin TFG.Typ t,HasSin TFG.Typ a) => String -> Exp g t -> Exp g a -> Bool
hasTag s ex ee = numTag s ex ee /= 0

numTag :: forall a t g. (HasSin TFG.Typ a , HasSin TFG.Typ t) =>
          String -> Exp g t -> Exp g a -> Int
numTag s ex ee = let t = sin :: TFG.Typ a in case ee of
  Tag x e
   | s == x    -> case eqlSin (sinTyp ex) t of
      Rgt Rfl  -> if eql ex e
                  then (1 :: Int)
                  else numTag s ex e
      _        -> impossible
   | otherwise -> numTag s ex e
  _            -> $(recAppMQ 'ee ''Exp (const [| 0 :: Int |]) ['Tag]
     [| \ _x -> (0 :: Int) |] [| (+) |] [| (+) |] (trvWrp 't)
    (\ tt -> if
     | matchQ tt [t| Exp (t ': t) t |]     -> [| numTag s (sucAll ex) |]
     | matchQ tt [t| Exp a a |]            -> [| numTag s ex |]
     | otherwise                           -> [| const (0 :: Int)  |]))


findTags :: forall g t. HasSin TFG.Typ t =>
            Exp g t -> [(String , Exs1 (Exp g) TFG.Typ)]
findTags ee = let t = sin :: TFG.Typ t in case ee of
  Tag x e   -> (x , Exs1 e (sinTyp e)) : findTags e
  _         -> $(recAppMQ 'ee ''Exp (const [| [] |]) ['Tag]
    [| \ _x -> [] |] [| (++) |] [| (++) |] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp (t ': t) t |]     -> [| findTagsF |]
    | matchQ tt [t| Exp t t |]            -> [| findTags |]
    | otherwise                           -> [| const [] |]))

findTagsF :: HasSin TFG.Typ b => Exp (a ': g) b -> [(String , Exs1 (Exp g) TFG.Typ)]
findTagsF e = foldr (\ (x , Exs1 eg t) xxs -> case prdAllM eg of
                                                Nothing ->  xxs
                                                Just eg' -> (x , Exs1 eg' t) : xxs) [] (findTags e)
-}

cseOne3 :: (HasSin TFG.Typ t , HasSin TFG.Typ a , HasSin TFG.Typ b , HasSin TFG.Typ c) =>
           (Exp g a -> Exp g b -> Exp g c -> Exp g t) ->
           Exp g a -> Exp g b -> Exp g c -> Chg (Exp g t)
cseOne3 k l m n  = case findSubterms l of
    tgs@(_:_) -> let f tss = case tss of
                       [] ->  k <$> cseOne l <*> cseOne m <*> cseOne n
                       (Exs1 ex tx : ts) -> case getPrfHasSin tx of
                                     PrfHasSin -> if hasSubterm ex m || hasSubterm ex n
                                                  then chg (Let ex (absSubterm (Var Zro) (sucAll ex) (sucAll (k l m n))))
                                                  else f ts
                 in f tgs
    []        -> case findSubterms m of
      tgs@(_:_) -> let f tss = case tss of
                        [] ->  k <$> cseOne l <*> cseOne m <*> cseOne n
                        (Exs1 ex tx : ts) -> case getPrfHasSin tx of
                                      PrfHasSin -> if hasSubterm ex n
                                                   then chg (Let ex (absSubterm (Var Zro) (sucAll ex) (sucAll (k l m n))))
                                                   else f ts
                   in f tgs
      [] -> k <$> cseOne l <*> cseOne m <*> cseOne n



cseOne2 :: (HasSin TFG.Typ a, HasSin TFG.Typ b, HasSin TFG.Typ c) =>
           (Exp g a -> Exp g b -> Exp g c) ->
            Exp g a -> Exp g b -> Chg (Exp g c)
cseOne2 k m n = case findSubterms m of
    tgs@(_:_) -> let f tss = case tss of
                       [] ->  k <$> cseOne m <*> cseOne n
                       (Exs1 ex tx : ts) -> case getPrfHasSin tx of
                                     PrfHasSin -> if hasSubterm ex n
                                                  then chg (Let ex (absSubterm (Var Zro) (sucAll ex) (sucAll (k m n))))
                                                  else f ts
                 in f tgs
    [] -> k <$> cseOne m <*> cseOne n

cseOne2F :: (HasSin TFG.Typ a, HasSin TFG.Typ b, HasSin TFG.Typ c) =>
           (Exp g a -> Exp (t ': g) b -> Exp g c) ->
            Exp g a -> Exp (t ': g) b -> Chg (Exp g c)
cseOne2F k m n = case findSubterms m of
    tgs@(_:_) -> let f tss = case tss of
                       [] ->  k <$> cseOne m <*> cseOne n
                       (Exs1 ex tx : ts) -> case getPrfHasSin tx of
                                     PrfHasSin -> if hasSubterm (sucAll ex) n
                                                  then chg (Let ex (absSubterm (Var Zro) (sucAll ex) (sucAll (k m n))))
                                                  else f ts
                 in f tgs
    [] -> k <$> cseOne m <*> cseOne n

absSubterm :: forall t t' g.
          (HasSin TFG.Typ t , HasSin TFG.Typ t') =>
          Exp g t' -> Exp g t' -> Exp g t -> Exp g t
absSubterm xx ex ee = let t = sin :: TFG.Typ t in
  case eqlSin (sinTyp ex) t of
    Rgt Rfl | eql ex ee -> xx
    _  -> $(genOverloadedW 'ee ''Exp  [] (trvWrp 't)
      (\ tt -> if
           | matchQ tt [t| Exp (t ': t) t |]     -> [| absSubterm (sucAll xx) (sucAll ex) |]
           | matchQ tt [t| Exp t t |]            -> [| absSubterm xx ex |]
           | otherwise                           -> [| id |]))


hasSubterm :: (HasSin TFG.Typ t,HasSin TFG.Typ a) => Exp g t -> Exp g a -> Bool
hasSubterm ex ee = numSubterm ex ee /= (0 :: Int)

numSubterm :: forall a t g. (HasSin TFG.Typ a , HasSin TFG.Typ t) =>
              Exp g t -> Exp g a -> Int
numSubterm ex ee = let t = sin :: TFG.Typ a in
                   (case eqlSin (sinTyp ex) t of
                       Rgt Rfl  -> if eql ex ee
                                   then (1 :: Int)
                                   else 0
                       _        -> 0) + $(recAppMQ 'ee ''Exp (const [| 0 :: Int |]) []
     [| \ _x -> (0 :: Int) |] [| (+) |] [| (+) |] (trvWrp 't)
    (\ tt -> if
     | matchQ tt [t| Exp (t ': t) t |]     -> [| numSubterm (sucAll ex) |]
     | matchQ tt [t| Exp a a |]            -> [| numSubterm ex |]
     | otherwise                           -> [| const (0 :: Int)  |]))

findSubterms :: forall g t. HasSin TFG.Typ t =>
                Exp g t -> [Exs1 (Exp g) TFG.Typ]
findSubterms ee = let t = sin :: TFG.Typ t in
                  (if not (isVal ee) && not (partialApp ee)
                   then ((Exs1 ee (sinTyp ee)) :)
                   else id)
 $(recAppMQ 'ee ''Exp (const [| [] |]) []
    [| \ _x -> [] |] [| (++) |] [| (++) |] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp (t ': t) t |]     -> [| findSubtermsF |]
    | matchQ tt [t| Exp t t |]            -> [| findSubterms |]
    | otherwise                           -> [| const [] |]))

findSubtermsF :: HasSin TFG.Typ b => Exp (a ': g) b -> [Exs1 (Exp g) TFG.Typ]
findSubtermsF e = foldr (\ (Exs1 eg t) xxs -> case prdAllM eg of
                                                Nothing  ->  xxs
                                                Just eg' -> (Exs1 eg' t) : xxs) [] (findSubterms e)

partialApp :: HasSin TFG.Typ a => Exp g a -> Bool
partialApp (App l _) = case sinTyp l of
  TFG.Arr _ (TFG.Arr _ _) -> True
  _                       -> False
partialApp _         = False

module Tests.Conversion where

import QHaskell.MyPrelude

import qualified Language.Haskell.TH.Syntax              as TH
import qualified QHaskell.Expression.ADTUntypedNamed     as FAUN
import qualified QHaskell.Expression.ADTUntypedDebruijn  as FAUD
import qualified QHaskell.Expression.GADTTyped           as FGTD
import qualified QHaskell.Expression.GADTFirstOrder      as FGFO
import qualified QHaskell.Expression.GADTHigherOrder     as FGHO
import qualified QHaskell.Expression.ADTValue            as FAV
import qualified QHaskell.Expression.GADTValue           as FGV
import qualified Tests.TemplateHaskell     as TH
import qualified Tests.ADTUntypedNamed     as FAUN
import qualified Tests.ADTUntypedDebruijn  as FAUD
import qualified Tests.GADTTyped           as FGTD
import qualified Tests.GADTFirstOrder      as FGFO
import qualified Tests.GADTHigherOrder     as FGHO
import qualified QHaskell.Type.ADT                       as TFA
import qualified QHaskell.Type.GADT                      as TFG
import qualified QHaskell.Environment.Map                         as EM
import qualified QHaskell.Environment.Plain                       as EP
import qualified QHaskell.Environment.Scoped                      as ES
import qualified QHaskell.Environment.Typed                       as ET
import QHaskell.Conversion
import QHaskell.Variable.Conversion                     ()
import QHaskell.Environment.Conversion                  ()
import QHaskell.Type.Conversion                ()
import QHaskell.Expression.Conversion          ()
import qualified QHaskell.Nat.ADT as NA
import QHaskell.Expression.Utils.TemplateHaskell

type One    = NA.Suc NA.Zro
type Add    = Arr Int (Arr Int Int)
type EnvAdd = Add ': '[]

typAddG :: TFG.Typ Add
typAddG = (TFG.Arr TFG.Int (TFG.Arr TFG.Int TFG.Int))

envAddTypG :: ET.Env TFG.Typ EnvAdd
envAddTypG =  ET.Ext typAddG ET.Emp

vec :: ES.Env One TH.Name
vec = ES.Ext (stripNameSpace 'TH.add) ES.Emp

envAddValG :: ET.Env FGV.Exp EnvAdd
envAddValG = ET.Ext (FGV.Exp (+)
                       :: FGV.Exp (Arr Int (Arr Int Int)))
             ET.Emp

envAddValV :: ES.Env One FAV.Exp
envAddValV = ES.Ext (FAV.lft ((+) :: Int -> Int -> Int)) ES.Emp

envAddValA :: EP.Env FAV.Exp
envAddValA = (FAV.lft ((+) :: Int -> Int -> Int)) : []

envAddValM :: EM.Env TH.Name FAV.Exp
envAddValM = (stripNameSpace 'TH.add , FAV.lft ((+) :: Int -> Int -> Int)) : []

cnvFGHO :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGHO.Exp EnvAdd Int) => e -> Int -> Bool
cnvFGHO e j = case runNamM
              (do e' :: FGHO.Exp EnvAdd  Int <- cnv (e , envAddTypG,vec)
                  curry cnv e' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _     -> False

cnvFGFO :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGFO.Exp EnvAdd Int) => e -> Int -> Bool
cnvFGFO e j = case runNamM
              (do e' :: FGFO.Exp EnvAdd Int <- cnv (e , envAddTypG ,vec)
                  curry cnv e' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _               -> False

cnvFGTD :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGTD.Exp One TFA.Typ) => e -> Int -> Bool
cnvFGTD e j = case runNamM
              (do e' :: FGTD.Exp One TFA.Typ <- cnv (e , envAddTypG , vec)
                  curry cnv e' envAddValV) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

cnvFAUD :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           FAUD.Exp => e -> Int -> Bool
cnvFAUD e j = case runNamM
              (do e' :: FAUD.Exp <- cnv (e , envAddTypG , vec)
                  curry cnv e' envAddValA) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

cnvFAUN :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FAUN.Exp TH.Name) => e -> Int -> Bool
cnvFAUN e j = case runNamM
              (do e' :: FAUN.Exp TH.Name <- cnv (e , envAddTypG , vec)
                  curry cnv e' envAddValM) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

test :: Bool
test = cnvFAUN TH.four   4 && cnvFAUN FAUN.four 4 &&

       cnvFAUD TH.four   4 && cnvFAUD FAUN.four 4 && cnvFAUD FAUD.four 4 &&

       cnvFGTD TH.four   4 && cnvFGTD FAUN.four 4 && cnvFGTD FAUD.four 4 &&
       cnvFGTD FGTD.four 4 &&

       cnvFGFO TH.four   4 && cnvFGFO FAUN.four 4 && cnvFGFO FAUD.four 4 &&
       cnvFGFO FGTD.four 4 && cnvFGFO FGFO.four 4 && cnvFGFO FGHO.four 4 &&

       cnvFGHO TH.four   4 && cnvFGHO FAUN.four 4 && cnvFGHO FAUD.four 4 &&
       cnvFGHO FGTD.four 4 && cnvFGHO FGFO.four 4 &&
       cnvFGHO FGHO.four 4

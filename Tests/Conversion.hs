module Tests.Conversion where

import QHaskell.MyPrelude

import qualified Language.Haskell.TH.Syntax              as TH
import qualified QHaskell.Expression.ADTUntypedNamed     as AUN
import qualified QHaskell.Expression.ADTUntypedDebruijn  as AUD
import qualified QHaskell.Expression.GADTTyped           as GTD
import qualified QHaskell.Expression.GADTFirstOrder      as GFO
import qualified QHaskell.Expression.GADTHigherOrder     as GHO
import qualified QHaskell.Expression.ADTValue            as FAV
import qualified QHaskell.Expression.GADTValue           as FGV
import qualified Tests.TemplateHaskell     as TH
import qualified Tests.ADTUntypedNamed     as AUN
import qualified Tests.ADTUntypedDebruijn  as AUD
import qualified Tests.GADTTyped           as GTD
import qualified Tests.GADTFirstOrder      as GFO
import qualified Tests.GADTHigherOrder     as GHO
import qualified QHaskell.Type.ADT                       as TA
import qualified QHaskell.Type.GADT                      as TG
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
type Add    = Word32 -> Word32 -> Word32
type EnvAdd = '[Add]

typAddG :: TG.Typ Add
typAddG = (TG.Arr TG.Wrd (TG.Arr TG.Wrd TG.Wrd))

envAddTypG :: ET.Env TG.Typ EnvAdd
envAddTypG =  ET.Ext typAddG ET.Emp

vec :: ES.Env One TH.Name
vec = ES.Ext (stripNameSpace 'TH.add) ES.Emp

envAddValG :: ET.Env FGV.Exp EnvAdd
envAddValG = ET.Ext (FGV.Exp (+)
                       :: FGV.Exp (Word32 -> Word32 -> Word32))
             ET.Emp

envAddValV :: ES.Env One FAV.Exp
envAddValV = ES.Ext (FAV.lft ((+) :: Word32 -> Word32 -> Word32)) ES.Emp

envAddValA :: EP.Env FAV.Exp
envAddValA = (FAV.lft ((+) :: Word32 -> Word32 -> Word32)) : []

envAddValM :: EM.Env TH.Name FAV.Exp
envAddValM = (stripNameSpace 'TH.add , FAV.lft ((+) :: Word32 -> Word32 -> Word32)) : []

cnvGHO :: Cnv (e , ET.Env TG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (GHO.Exp EnvAdd Word32) => e -> Word32 -> Bool
cnvGHO e j = case runNamM
              (do e' :: GHO.Exp EnvAdd  Word32 <- cnv (e , envAddTypG,vec)
                  curry cnv e' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _     -> False

cnvGFO :: Cnv (e , ET.Env TG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (GFO.Exp EnvAdd '[] Word32) => e -> Word32 -> Bool
cnvGFO e j = case runNamM
              (do e' :: GFO.Exp EnvAdd '[] Word32 <- cnv (e , envAddTypG ,vec)
                  cnv (e' , (envAddValG ,ET.Emp :: ET.Env FGV.Exp '[]))) of
           Rgt (FGV.Exp i) -> i == j
           _               -> False

cnvGTD :: Cnv (e , ET.Env TG.Typ EnvAdd , ES.Env One TH.Name)
           (GTD.Exp One NA.Zro TA.Typ) => e -> Word32 -> Bool
cnvGTD e j = case runNamM
              (do e' :: GTD.Exp One NA.Zro TA.Typ <- cnv (e , envAddTypG , vec)
                  cnv (e' , (envAddValV,ES.Emp :: ES.Env NA.Zro FAV.Exp))) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

cnvAUD :: Cnv (e , ET.Env TG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           AUD.Exp => e -> Word32 -> Bool
cnvAUD e j = case runNamM
              (do e' :: AUD.Exp <- cnv (e , envAddTypG , vec)
                  cnv (e' , (envAddValA , [] :: EP.Env FAV.Exp))) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

cnvAUN :: Cnv (e , ET.Env TG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (AUN.Exp TH.Name) => e -> Word32 -> Bool
cnvAUN e j = case runNamM
              (do e' :: AUN.Exp TH.Name <- cnv (e , envAddTypG , vec)
                  cnv (e' , (envAddValM,[] :: EM.Env TH.Name FAV.Exp))) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

test :: Bool
test = cnvAUN TH.four   4 && cnvAUN AUN.four 4 &&

       cnvAUD TH.four   4 && cnvAUD AUN.four 4 && cnvAUD AUD.four 4 &&

       cnvGTD TH.four   4 && cnvGTD AUN.four 4 && cnvGTD AUD.four 4 &&
       cnvGTD GTD.four 4 &&

       cnvGFO TH.four   4 && cnvGFO AUN.four 4 && cnvGFO AUD.four 4 &&
       cnvGFO GTD.four 4 && cnvGFO GFO.four 4 && cnvGFO GHO.four 4 &&

       cnvGHO TH.four   4 && cnvGHO AUN.four 4 && cnvGHO AUD.four 4 &&
       cnvGHO GTD.four 4 && cnvGHO GFO.four 4 &&
       cnvGHO GHO.four 4

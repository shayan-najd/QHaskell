module Test where

import QHaskell.MyPrelude

import qualified Tests.ADTUntypedNamed     as AUN
import qualified Tests.ADTUntypedDebruijn  as AUD
import qualified Tests.GADTTyped           as GTD
import qualified Tests.GADTFirstOrder      as GFO
import qualified Tests.GADTHigherOrder     as GHO
import qualified Tests.Conversion          as CNV

import QHaskell.Normalisation ()
import QHaskell.Simplification ()
import QHaskell.CSE ()

import QHaskell.Expression.Utils.Reuse.GADTHigherOrder ()
import QHaskell.Expression.Utils.Equality.GADTHigherOrder ()
import QHaskell.Expression.Utils.Show.GADTFirstOrder ()
import QHaskell.Expression.Utils.Show.GADTHigherOrder ()
import QHaskell.Expression.Utils.ADTUntypedDebruijn ()
import QHaskell.Expression.Utils.ADTUntypedNamed ()
import QHaskell.Expression.Utils.GADTTyped ()

main :: IO ()
main = print (if AUN.test  && AUD.test && GTD.test  &&
                 GFO.test  && GHO.test &&
                 CNV.test
              then "Pass!"
              else "Fail!")

-- Todo:
-- * Check for exotic terms
-- * Weakening of HOAS
--    + getting rid of Tmp
-- * Bidirectional translations
-- * Using Type classes to do lifting and colifting for ADTValue and GADTValue
--   to reuse Vanilla Prelude
-- * check for all exhaustive partterns and transform them
-- * Conversion of GHO (x :-> y) ~> (MFS x -> MFS y)
-- * Free Fusion for Church / Ahman's Containers
-- * Supporting F
-- * Scope Proofing Quotations (e.g. Sam's misunderstanding) [EncodingTypes.txt]
-- * Support for Syntactic Suggar in Quotations (e.g. use TH-Desugar)
-- * Add sqrt and memorize (for Float) to Preludes that do not have it
-- * Write the code required for memorize
-- * Use macros for polymorphic datatypes
-- * Generate polymorphic datatypes
-- * Check all external imports are only via MyPrelude
-- * Shift Whole Pipline to the compile time!
-- * Use let in FFT and CRC
-- * Use eta

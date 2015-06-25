module Examples.VHDL.Examples where

import Prelude
import QHaskell
import Examples.VHDL.FrontEnd
import Examples.VHDL.Glue

always :: Qt Bool
always = [|| True ||]


-- ToDo: Due to how TH-Desugar was originally written
-- I could not write \ _ -> True since
-- it gives us a silly case expression.
-- I should update TH-Desugar.
alwaysF :: Qt (Bool -> Bool)
alwaysF = [|| \ _x -> True ||]

alwaysFVHDL :: IO ()
alwaysFVHDL = putStrLn $ compile alwaysF

module Structure
    (Structure(..),
     checkStructure)
where

import PetriNet
import Data.List (intersect)

data Structure = FreeChoice

instance Show Structure where
        show FreeChoice = "free choice"

checkStructure :: PetriNet -> Structure -> Bool
checkStructure net FreeChoice =
            all freeChoiceCond [(p,s) | p <- places net, s <- places net]
        where freeChoiceCond (p,s) =
                  let ppost = post net p
                      spost = post net s
                  in null (ppost `intersect` spost) || ppost == spost


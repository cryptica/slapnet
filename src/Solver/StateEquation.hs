module Solver.StateEquation
    (checkStateEquation,checkStateEquationSat,
     markedPlacesFromAssignment)
where

import Data.SBV

import PetriNet
import Property
import Solver
import Solver.Formula

placeConstraints :: PetriNet -> ModelSI -> SBool
placeConstraints net m = bAnd $ map checkPlaceEquation $ places net
        where checkPlaceEquation p =
                let incoming = map addTransition $ lpre net p
                    outgoing = map addTransition $ lpost net p
                    pinit = literal $ initial net p
                in  pinit + sum incoming - sum outgoing .== mVal m p
              addTransition (t,w) = literal w * mVal m t

nonnegativityConstraints ::  ModelSI -> SBool
nonnegativityConstraints m = bAnd $ map (.>= 0) $ mValues m

checkTraps :: [[String]] -> ModelSI -> SBool
checkTraps traps m = bAnd $ map checkTrapDelta traps
        where checkTrapDelta trap = sum (map (mVal m) trap) .>= 1

checkStateEquation :: PetriNet -> Formula -> [[String]] -> ModelSI -> SBool
checkStateEquation net f traps m =
        placeConstraints net m &&&
        nonnegativityConstraints m &&&
        checkTraps traps m &&&
        evaluateFormula f m

checkStateEquationSat :: PetriNet -> Formula -> [[String]] ->
        ([String], ModelSI -> SBool)
checkStateEquationSat net f traps =
        (places net ++ transitions net, checkStateEquation net f traps)

markedPlacesFromAssignment :: PetriNet -> ModelI -> [String]
markedPlacesFromAssignment net a = filter (cElem a) $ places net

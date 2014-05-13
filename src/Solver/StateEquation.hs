module Solver.StateEquation
    (checkStateEquation,checkStateEquationSat,
     markedPlacesFromAssignment)
where

import Data.SBV
import qualified Data.Map as M

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
                in  pinit + sum incoming - sum outgoing .== (m M.! p)
              addTransition (t,w) = literal w * (m M.! t)

nonnegativityConstraints ::  ModelSI -> SBool
nonnegativityConstraints m = bAnd $ map (.>= 0) $ M.elems m

checkTraps :: [[String]] -> ModelSI -> SBool
checkTraps traps m = bAnd $ map checkTrapDelta traps
        where checkTrapDelta trap = sum (map (m M.!) trap) .>= 1

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
markedPlacesFromAssignment net a = filter (( > 0) . (a M.!)) $ places net

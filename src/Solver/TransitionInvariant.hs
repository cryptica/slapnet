module Solver.TransitionInvariant
    (checkTransitionInvariant,checkTransitionInvariantSat,
     firedTransitionsFromAssignment)
where

import Data.SBV
import qualified Data.Map as M

import PetriNet
import Property
import Solver
import Solver.Formula

tInvariantConstraints :: PetriNet -> ModelSI -> SBool
tInvariantConstraints net m =
            bAnd $ map checkTransitionEquation $ places net
        where checkTransitionEquation p =
                let incoming = map addTransition $ lpre net p
                    outgoing = map addTransition $ lpost net p
                in  sum outgoing - sum incoming .>= 0
              addTransition (t,w) = literal w * (m M.! t)

finalInvariantConstraints :: ModelSI -> SBool
finalInvariantConstraints m = sum (M.elems m) .> 0

nonnegativityConstraints :: ModelSI -> SBool
nonnegativityConstraints m = bAnd $ map (.>= 0) $ M.elems m

checkTransitionInvariant :: PetriNet -> Formula -> ModelSI -> SBool
checkTransitionInvariant net f m =
        tInvariantConstraints net m &&&
        nonnegativityConstraints m &&&
        finalInvariantConstraints m &&&
        evaluateFormula f m

checkTransitionInvariantSat :: PetriNet -> Formula -> ([String], ModelSI -> SBool)
checkTransitionInvariantSat net f =
        (transitions net, checkTransitionInvariant net f)

firedTransitionsFromAssignment :: ModelI -> [String]
firedTransitionsFromAssignment a = M.keys $ M.filter ( > 0) a

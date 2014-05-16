module Solver.TransitionInvariant
    (checkTransitionInvariant,checkTransitionInvariantSat,
     firedTransitionsFromAssignment)
where

import Data.SBV

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
                in  sum incoming - sum outgoing .>= 0
              addTransition (t,w) = literal w * mVal m t

finalInvariantConstraints :: ModelSI -> SBool
finalInvariantConstraints m = sum (mValues m) .> 0

nonnegativityConstraints :: ModelSI -> SBool
nonnegativityConstraints m = bAnd $ map (.>= 0) $ mValues m

checkSComponentTransitions :: [([String],[String])] -> ModelSI -> SBool
checkSComponentTransitions strans m = bAnd $ map checkInOut strans
        where checkInOut (sOut,sIn) =
                bAnd (map (\t -> mVal m t .> 0) sOut) ==>
                bOr (map (\t -> mVal m t .> 0) sIn)

checkTransitionInvariant :: PetriNet -> Formula -> [([String],[String])] ->
        ModelSI -> SBool
checkTransitionInvariant net f strans m =
        tInvariantConstraints net m &&&
        nonnegativityConstraints m &&&
        finalInvariantConstraints m &&&
        checkSComponentTransitions strans m &&&
        evaluateFormula f m

checkTransitionInvariantSat :: PetriNet -> Formula -> [([String],[String])] ->
        ([String], ModelSI -> SBool)
checkTransitionInvariantSat net f strans =
        (transitions net, checkTransitionInvariant net f strans)

firedTransitionsFromAssignment :: ModelI -> [String]
firedTransitionsFromAssignment = mElemsWith (> 0)

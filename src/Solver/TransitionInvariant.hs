module Solver.TransitionInvariant
    (checkTransitionInvariant,checkTransitionInvariantSat,
     firedTransitionsFromAssignment)
where

import Data.SBV

import PetriNet
import Property
import Solver
import Solver.SComponent
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

checkSComponentTransitions :: [SCompCut] -> ModelSI -> SBool
checkSComponentTransitions comps m =
            bAnd $ map (bOr . map checkCompsCut) comps
        where checkCompsCut (ts,w) =
              -- TODO: check how changing the representation changes result
                let tc t = mVal m t .> 0
                in  if w then bnot (bOr (map tc ts)) else bOr (map tc ts)

checkTransitionInvariant :: PetriNet -> Formula ->
        [SCompCut] -> ModelSI -> SBool
checkTransitionInvariant net f strans m =
        tInvariantConstraints net m &&&
        nonnegativityConstraints m &&&
        finalInvariantConstraints m &&&
        checkSComponentTransitions strans m &&&
        evaluateFormula f m

checkTransitionInvariantSat :: PetriNet -> Formula ->
        [SCompCut] -> ([String], ModelSI -> SBool)
checkTransitionInvariantSat net f strans =
        (transitions net, checkTransitionInvariant net f strans)

firedTransitionsFromAssignment :: ModelI -> [String]
firedTransitionsFromAssignment = mElemsWith (> 0)

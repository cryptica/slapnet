module Solver.TransitionInvariant
    (checkTransitionInvariantSat)
where

import Data.SBV

import Util
import PetriNet
import Property
import Solver
import Solver.Formula

tInvariantConstraints :: PetriNet -> SIMap Transition -> SBool
tInvariantConstraints net x =
            bAnd $ map checkTransitionEquation $ places net
        where checkTransitionEquation p =
                  let incoming = map addTransition $ lpre net p
                      outgoing = map addTransition $ lpost net p
                  in  sum incoming - sum outgoing .>= 0
              addTransition (t,w) = literal w * val x t

finalInvariantConstraints :: SIMap Transition -> SBool
finalInvariantConstraints x = sum (vals x) .> 0

nonnegativityConstraints :: SIMap Transition -> SBool
nonnegativityConstraints x = bAnd $ map (.>= 0) (vals x)

-- TODO: check how changing the representation changes result
checkCuts :: [Cut] -> SIMap Transition -> SBool
checkCuts cuts x = bAnd $ map checkCut cuts
        where checkCut (ts, u) =
                  let cPre = map ((bnot . bOr) . map (positiveVal x)) ts
                      cPost = map (positiveVal x) u
                  in  bOr cPre ||| bOr cPost

checkTransitionInvariant :: PetriNet -> Formula Transition ->
        [Cut] -> SIMap Transition -> SBool
checkTransitionInvariant net f cuts x =
        tInvariantConstraints net x &&&
        nonnegativityConstraints x &&&
        finalInvariantConstraints x &&&
        checkCuts cuts x &&&
        evaluateFormula f x

checkTransitionInvariantSat :: PetriNet -> Formula Transition -> [Cut] ->
        ConstraintProblem Integer FiringVector
checkTransitionInvariantSat net f cuts =
        let x = makeVarMap $ transitions net
        in  ("transition invariant constraints", "transition invariant",
            getNames x,
            \fm -> checkTransitionInvariant net f cuts (fmap fm x),
            \fm -> firingVectorFromAssignment (fmap fm x))

firingVectorFromAssignment :: IMap Transition -> FiringVector
firingVectorFromAssignment = makeVector


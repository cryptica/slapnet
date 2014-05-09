module Solver.TransitionInvariant
    (checkTransitionInvariant,checkTransitionInvariantSat)
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
                let incoming = map addPlace $ lpre net p
                    outgoing = map addPlace $ lpost net p
                in  sum outgoing - sum incoming .>= 0
              addPlace (t,w) = literal w * (m M.! t)

nonnegativityConstraints :: PetriNet -> ModelSI -> SBool
nonnegativityConstraints net m =
            bAnd $ map checkT $ transitions net
        where checkT t = (m M.! t) .>= 0

checkTransitionInvariant :: PetriNet -> Formula -> ModelSI -> SBool
checkTransitionInvariant net f m =
        tInvariantConstraints net m &&&
        nonnegativityConstraints net m &&&
        evaluateFormula f m

checkTransitionInvariantSat :: PetriNet -> Formula -> ([String], ModelSI -> SBool)
checkTransitionInvariantSat net f =
        (transitions net, checkTransitionInvariant net f)

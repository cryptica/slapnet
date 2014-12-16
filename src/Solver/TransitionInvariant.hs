module Solver.TransitionInvariant
    (checkTransitionInvariantSat)
where

import Data.SBV
import Control.Monad

import PetriNet
import Property
import Solver
--import Solver.SComponent
import Solver.Formula

tInvariantConstraints :: PetriNet -> VarMap Transition -> IntConstraint
tInvariantConstraints net x =
            liftM bAnd $ mapM checkTransitionEquation $ places net
        where checkTransitionEquation p = do
                incoming <- mapM addTransition $ lpre net p
                outgoing <- mapM addTransition $ lpost net p
                return $ sum incoming - sum outgoing .>= 0
              addTransition (t,w) = liftM (literal w *) (val x t)

finalInvariantConstraints :: VarMap Transition -> IntConstraint
finalInvariantConstraints x = do
        xs <- vals x
        return $ sum xs .> 0

nonnegativityConstraints :: VarMap Transition -> IntConstraint
nonnegativityConstraints x = do
        xs <- vals x
        return $ bAnd $ map (.>= 0) xs
{-
checkSComponentTransitions :: [Cut] -> VarMap Transition -> IntConstraint
checkSComponentTransitions cuts x =
            bAnd $ map (bOr . map checkCompsCut) comps
        where checkCompsCut (ts,w) =
              -- TODO: check how changing the representation changes result
                let tc t = mVal m t .> 0
                in  if w then bnot (bOr (map tc ts)) else bOr (map tc ts)
-}
checkTransitionInvariant :: PetriNet -> Formula Transition ->
        [Cut] -> VarMap Transition -> IntConstraint
checkTransitionInvariant net f cuts x = do
        c1 <- tInvariantConstraints net x
        c2 <- nonnegativityConstraints x
        c3 <- finalInvariantConstraints x
        --c4 <- checkSComponentTransitions cuts x
        c5 <- evaluateFormula f x
        return $ c1 &&& c2 &&& c3 &&& c3 &&& c5

checkTransitionInvariantSat :: PetriNet -> Formula Transition -> [Cut] ->
        ConstraintProblem Integer FiringVector
checkTransitionInvariantSat net f cuts =
       let x = makeVarMap $ transitions net
       in  ("transition invariant constraints", "transition invariant",
            getNames x,
            checkTransitionInvariant net f cuts x,
            firingVectorFromAssignment x)

firingVectorFromAssignment :: VarMap Transition -> IntResult FiringVector
firingVectorFromAssignment x =
        liftM makeVector $ valMap x


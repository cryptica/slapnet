module Solver.TransitionInvariant
    (checkTransitionInvariantSat)
where

import Data.SBV
import Control.Monad

import PetriNet
import Property
import Solver
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

-- TODO: check how changing the representation changes result
checkCuts :: [Cut] -> VarMap Transition -> IntConstraint
checkCuts cuts x =
            liftM bAnd $ mapM checkCut cuts
        where checkCut (ts, u) = do
                cPre <- mapM (liftM (bnot . bOr) . mapM positive) ts
                cPost <- mapM positive u
                return $ bOr cPre ||| bOr cPost
              positive t = liftM (.> 0) (val x t)

checkTransitionInvariant :: PetriNet -> Formula Transition ->
        [Cut] -> VarMap Transition -> IntConstraint
checkTransitionInvariant net f cuts x = do
        c1 <- tInvariantConstraints net x
        c2 <- nonnegativityConstraints x
        c3 <- finalInvariantConstraints x
        c4 <- checkCuts cuts x
        c5 <- evaluateFormula f x
        return $ c1 &&& c2 &&& c3 &&& c4 &&& c5

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


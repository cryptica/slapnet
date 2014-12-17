module Solver.StateEquation
    (checkStateEquationSat)
where

import Data.SBV
import Control.Monad

import Util
import PetriNet
import Property
import Solver
import Solver.Formula

placeConstraints :: PetriNet -> VarMap Place -> VarMap Transition -> IntConstraint
placeConstraints net m x =
            liftM bAnd $ mapM checkPlaceEquation $ places net
        where checkPlaceEquation p = do
                mp <- val m p
                incoming <- mapM addTransition $ lpre net p
                outgoing <- mapM addTransition $ lpost net p
                let pinit = literal $ initial net p
                return $ pinit + sum incoming - sum outgoing .== mp
              addTransition (t,w) = liftM (literal w *) (val x t)

nonNegativityConstraints :: PetriNet -> VarMap Place -> VarMap Transition ->
        IntConstraint
nonNegativityConstraints net m x = do
            mnn <- mapM (checkVal m) (places net)
            xnn <- mapM (checkVal x) (transitions net)
            return $ bAnd mnn &&& bAnd xnn
        where checkVal mapping n = do
                mn <- val mapping n
                return $ mn .>= 0

checkTraps :: [Trap] -> VarMap Place -> IntConstraint
checkTraps traps m = do
            tc <- mapM checkTrapDelta traps
            return $ bAnd tc
        where checkTrapDelta trap = do
                mts <- mapM (val m) trap
                return $ sum mts .>= 1

checkStateEquation :: PetriNet -> Formula Place ->
        VarMap Place -> VarMap Transition -> [Trap] ->
        IntConstraint
checkStateEquation net f m x traps = do
        c1 <- placeConstraints net m x
        c2 <- nonNegativityConstraints net m x
        c3 <- checkTraps traps m
        c4 <- evaluateFormula f m
        return $ c1 &&& c2 &&& c3 &&& c4

checkStateEquationSat :: PetriNet -> Formula Place -> [Trap] ->
        ConstraintProblem Integer Marking
checkStateEquationSat net f traps =
        let m = makeVarMap $ places net
            x = makeVarMap $ transitions net
        in  ("state equation", "marking",
             getNames m ++ getNames x,
             checkStateEquation net f m x traps,
             markingFromAssignment m)

markingFromAssignment :: VarMap Place -> IntResult Marking
markingFromAssignment m =
        liftM makeVector $ valMap m


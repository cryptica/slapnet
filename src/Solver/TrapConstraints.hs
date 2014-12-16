module Solver.TrapConstraints
    (checkTrapSat)
where

import Data.SBV
import Control.Monad
import qualified Data.Map as M

import PetriNet
import Solver

trapConstraints :: PetriNet -> VarMap Place -> BoolConstraint
trapConstraints net b =
            liftM bAnd $ mapM trapConstraint $ transitions net
        where trapConstraint t = do
                cPre <- mapM (val b) $ pre net t
                cPost <- mapM (val b) $ post net t
                return $ bOr cPre ==> bOr cPost

trapInitiallyMarked :: PetriNet -> VarMap Place -> BoolConstraint
trapInitiallyMarked net b =
        liftM bOr $ mapM (val b) $ initials net

trapUnassigned :: Marking -> VarMap Place -> BoolConstraint
trapUnassigned m b =
        liftM bAnd $ mapM (liftM bnot . val b) $ elems m

checkTrap :: PetriNet -> Marking -> VarMap Place -> BoolConstraint
checkTrap net m b = do
        c1 <- trapConstraints net b
        c2 <- trapInitiallyMarked net b
        c3 <- trapUnassigned m b
        return $ c1 &&& c2 &&& c3

checkTrapSat :: PetriNet -> Marking -> ConstraintProblem Bool Trap
checkTrapSat net m =
        let b = makeVarMap $ places net
        in  ("trap constraints", "trap",
             getNames b,
             checkTrap net m b,
             trapFromAssignment b)

trapFromAssignment :: VarMap Place -> BoolResult Trap
trapFromAssignment b = do
        bm <- valMap b
        return $ M.keys $ M.filter id bm


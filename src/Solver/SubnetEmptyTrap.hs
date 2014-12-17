module Solver.SubnetEmptyTrap
    (checkSubnetEmptyTrapSat)
where

import Data.SBV
import Control.Monad
import qualified Data.Map as M

import Util
import PetriNet
import Solver

subnetTrapConstraints :: PetriNet -> Marking -> FiringVector ->
        VarMap Place -> BoolConstraint
subnetTrapConstraints net m x b =
            liftM bAnd $ mapM trapConstraint $ elems x
        where placeConstraints = mapM (val b) . filter (\p -> value m p == 0)
              trapConstraint t = do
                cPre <- placeConstraints $ pre net t
                cPost <- placeConstraints $ post net t
                return $ bOr cPre ==> bOr cPost

properTrap :: VarMap Place -> BoolConstraint
properTrap b = liftM bOr $ vals b

checkSubnetEmptyTrap :: PetriNet -> Marking -> FiringVector ->
        VarMap Place -> BoolConstraint
checkSubnetEmptyTrap net m x b = do
        c1 <- subnetTrapConstraints net m x b
        c2 <- properTrap b
        return $ c1 &&& c2

checkSubnetEmptyTrapSat :: PetriNet -> Marking -> FiringVector ->
        ConstraintProblem Bool Trap
checkSubnetEmptyTrapSat net m x =
        let b = makeVarMap $ filter (\p -> value m p == 0) $ mpost net $ elems x
        in  ("subnet empty trap constraints", "trap",
            getNames b,
            checkSubnetEmptyTrap net m x b,
            trapFromAssignment b)

trapFromAssignment :: VarMap Place -> BoolResult Trap
trapFromAssignment b = do
        bm <- valMap b
        return $ M.keys $ M.filter id bm


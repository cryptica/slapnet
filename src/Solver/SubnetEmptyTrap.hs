module Solver.SubnetEmptyTrap
    (checkSubnetEmptyTrapSat)
where

import Data.SBV
import qualified Data.Map as M

import Util
import PetriNet
import Solver

subnetTrapConstraints :: PetriNet -> Marking -> FiringVector ->
        SBMap Place -> SBool
subnetTrapConstraints net m x b =
            bAnd $ map trapConstraint $ elems x
        where placeConstraints = mval b . filter (\p -> val m p == 0)
              trapConstraint t =
                  bOr (placeConstraints (pre net t)) ==>
                  bOr (placeConstraints (post net t))

properTrap :: SBMap Place -> SBool
properTrap b = bOr $ vals b

checkSubnetEmptyTrap :: PetriNet -> Marking -> FiringVector ->
        SBMap Place -> SBool
checkSubnetEmptyTrap net m x b =
        subnetTrapConstraints net m x b &&&
        properTrap b

checkSubnetEmptyTrapSat :: PetriNet -> Marking -> FiringVector ->
        ConstraintProblem Bool Trap
checkSubnetEmptyTrapSat net m x =
        let b = makeVarMap $ filter (\p -> val m p == 0) $ mpost net $ elems x
        in  ("subnet empty trap constraints", "trap",
            getNames b,
            \fm -> checkSubnetEmptyTrap net m x (fmap fm b),
            \fm -> trapFromAssignment (fmap fm b))

trapFromAssignment :: BMap Place -> Trap
trapFromAssignment b = M.keys $ M.filter id b


module Solver.TrapConstraints
    (checkTrap,checkTrapSat,
     trapFromAssignment
    )
where

import Data.SBV
import qualified Data.Map as M

import PetriNet
import Solver

trapConstraints :: PetriNet -> ModelSB -> SBool
trapConstraints net m =
            bAnd $ map trapConstraint $ transitions net
        where trapConstraint t =
                bOr (map (m M.!) $ pre net t) ==> bOr (map (m M.!) $ post net t)

trapInitiallyMarked :: PetriNet -> ModelSB -> SBool
trapInitiallyMarked net m =
        let marked = map fst $ filter (( > 0) . snd) $ initials net
        in  bOr $ map (m M.!) marked

trapUnassigned :: [String] -> ModelSB -> SBool
trapUnassigned assigned m = bAnd $ map (bnot . (m M.!)) assigned

checkTrap :: PetriNet -> [String] -> ModelSB -> SBool
checkTrap net assigned m =
        trapConstraints net m &&&
        trapInitiallyMarked net m &&&
        trapUnassigned assigned m

checkTrapSat :: PetriNet -> [String] -> ([String], ModelSB -> SBool)
checkTrapSat net assigned =
        (places net, checkTrap net assigned)

trapFromAssignment :: ModelB -> [String]
trapFromAssignment a = M.keys $ M.filter id a


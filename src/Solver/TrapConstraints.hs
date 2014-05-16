module Solver.TrapConstraints
    (checkTrap,checkTrapSat,
     trapFromAssignment
    )
where

import Data.SBV

import PetriNet
import Solver

trapConstraints :: PetriNet -> ModelSB -> SBool
trapConstraints net m =
            bAnd $ map trapConstraint $ transitions net
        where trapConstraint t =
                bOr (map (mElem m) $ pre net t) ==> bOr (map (mElem m) $ post net t)

trapInitiallyMarked :: PetriNet -> ModelSB -> SBool
trapInitiallyMarked net m =
        let marked = map fst $ filter (( > 0) . snd) $ initials net
        in  bOr $ map (mElem m) marked

trapUnassigned :: [String] -> ModelSB -> SBool
trapUnassigned assigned m = bAnd $ map (mNotElem m) assigned

checkTrap :: PetriNet -> [String] -> ModelSB -> SBool
checkTrap net assigned m =
        trapConstraints net m &&&
        trapInitiallyMarked net m &&&
        trapUnassigned assigned m

checkTrapSat :: PetriNet -> [String] -> ([String], ModelSB -> SBool)
checkTrapSat net assigned =
        (places net, checkTrap net assigned)

trapFromAssignment :: ModelB -> [String]
trapFromAssignment = mElemsWith id


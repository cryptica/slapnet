module Solver.TrapConstraints
    (checkTrap,checkTrapSat,
     trapFromAssignment
    )
where

import Z3.Monad
import Control.Monad

import PetriNet
import Solver

trapConstraints :: PetriNet -> MModelS -> Z3 ()
trapConstraints net m = mapM_ (assertCnstr <=< trapConstraint) $ transitions net
        where trapConstraint t = do
                lhs <- mkOr' (map (mVal m) $ pre net t)
                rhs <- mkOr' (map (mVal m) $ post net t)
                mkImplies lhs rhs

trapInitiallyMarked :: PetriNet -> MModelS -> Z3 ()
trapInitiallyMarked net m = assertCnstr =<< mkOr' (map (mVal m) (initials net))

trapUnassigned :: [String] -> MModelS -> Z3 ()
trapUnassigned assigned m = mapM_ (assertCnstr <=< (mkNot . mVal m)) assigned

checkTrap :: PetriNet -> [String] -> MModelS -> Z3 ()
checkTrap net assigned m = do
        trapConstraints net m
        trapInitiallyMarked net m
        trapUnassigned assigned m

checkTrapSat :: PetriNet -> [String] -> ([String], MModelS -> Z3 ())
checkTrapSat net assigned =
        (places net, checkTrap net assigned)

trapFromAssignment :: MModelB -> [String]
trapFromAssignment = mElemsWith (\x -> case x of Just True -> True
                                                 _ -> False )


module Solver.StateEquation
    (checkStateEquation,checkStateEquationSat,
     markedPlacesFromAssignment)
where

import Z3.Monad
import Control.Monad

import PetriNet
import Property
import Solver
import Solver.Formula

placeConstraints :: PetriNet -> MModelS -> Z3 ()
placeConstraints net m = mapM_ (assertCnstr <=< checkPlaceEquation) $ places net
        where checkPlaceEquation p = do
                incoming <- mapM (addTransition   1 ) $ lpre net p
                outgoing <- mapM (addTransition (-1)) $ lpost net p
                pinit <- mkVal $ initial net p
                sums <- mkAdd (pinit:(incoming ++ outgoing))
                mkEq sums (mVal m p)
              addTransition fac (t,w) =
                  mkVal (fac*w) >>= \w' -> mkMul [w', mVal m t]

nonnegativityConstraints :: MModelS -> Z3 ()
nonnegativityConstraints m = mapM_ (assertCnstr <=< geZero) $ mValues m
        where geZero v = mkGe v =<< mkVal (0::Integer)

checkTraps :: [[String]] -> MModelS -> Z3 ()
checkTraps traps m = mapM_ (assertCnstr <=< checkTrap) traps
        where checkTrap trap = mkAdd (map (mVal m) trap) >>=
                  (\v -> mkGe v =<< mkVal (1::Integer))

checkStateEquation :: PetriNet -> Formula -> [[String]] -> MModelS -> Z3 ()
checkStateEquation net f traps m = do
        placeConstraints net m
        nonnegativityConstraints m
        checkTraps traps m
        assertCnstr =<< evaluateFormula f m

checkStateEquationSat :: PetriNet -> Formula -> [[String]] ->
        ([String], MModelS -> Z3 ())
checkStateEquationSat net f traps =
        (places net ++ transitions net, checkStateEquation net f traps)

markedPlacesFromAssignment :: PetriNet -> MModelI -> [String]
markedPlacesFromAssignment net a = filter (cElem a) $ places net

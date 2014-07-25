module Solver.TransitionInvariant
    (checkTransitionInvariant,checkTransitionInvariantSat,
     firedTransitionsFromAssignment)
where

import Z3.Monad
import Control.Monad

import PetriNet
import Property
import Solver
import Solver.Formula

tInvariantConstraints :: PetriNet -> MModelS -> Z3 ()
tInvariantConstraints net m =
            mapM_ (assertCnstr <=< checkTransitionEquation) $ places net
        where checkTransitionEquation p = do
                incoming <- mapM (addTransition   1 ) $ lpre net p
                outgoing <- mapM (addTransition (-1)) $ lpost net p
                sums <- mkAdd' (incoming ++ outgoing)
                mkGe sums =<< mkVal (0::Integer)
              addTransition fac (t,w) =
                  mkVal (fac*w) >>= \w' -> mkMul [w', mVal m t]

finalInvariantConstraints :: MModelS -> Z3 ()
finalInvariantConstraints m = do
        sums <- mkAdd' (mValues m)
        assertCnstr =<< mkGt sums =<< mkVal (0::Integer)

nonnegativityConstraints :: MModelS -> Z3 ()
nonnegativityConstraints m = mapM_ (assertCnstr <=< geZero) $ mValues m
        where geZero v = mkGe v =<< mkVal (0::Integer)

checkSComponentTransitions :: [([String],[String])] -> MModelS -> Z3 ()
checkSComponentTransitions strans m = mapM_ (assertCnstr <=< checkInOut) strans
        where checkInOut (sOut,sIn) = do
                lhs <- mkAnd' =<<
                        mapM (\t -> mkGt (mVal m t) =<< mkVal (0::Integer)) sOut
                rhs <- mkOr'  =<<
                        mapM (\t -> mkGt (mVal m t) =<< mkVal (0::Integer)) sIn
                mkImplies lhs rhs

checkTransitionInvariant :: PetriNet -> Formula -> [([String],[String])] ->
        MModelS -> Z3 ()
checkTransitionInvariant net f strans m = do
        tInvariantConstraints net m
        nonnegativityConstraints m
        finalInvariantConstraints m
        checkSComponentTransitions strans m
        assertCnstr =<< evaluateFormula f m

checkTransitionInvariantSat :: PetriNet -> Formula -> [([String],[String])] ->
        ([String], MModelS -> Z3 ())
checkTransitionInvariantSat net f strans =
        (transitions net, checkTransitionInvariant net f strans)

firedTransitionsFromAssignment :: MModelI -> [String]
firedTransitionsFromAssignment = mElemsWith (> 0)

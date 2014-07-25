module Solver.SComponent
    (checkSComponent,checkSComponentSat,
     getSComponentOutIn)
where

import Z3.Monad
import Control.Monad
import Data.List (partition)

import PetriNet
import Solver

prime :: String -> String
prime = ('\'':)

checkPrePostPlaces :: PetriNet -> MModelS -> Z3 ()
checkPrePostPlaces net m =
            mapM_ (assertCnstr <=< checkPrePostPlace) $ places net
        where checkPrePostPlace p = do
                incoming <- mapM (mElem m) $ pre net p
                outgoing <- mapM (mElem m) $ post net p
                lhs <- mElem m p
                rhs <- mkAnd' (incoming ++ outgoing)
                mkImplies lhs rhs

checkPrePostTransitions :: PetriNet -> MModelS -> Z3 ()
checkPrePostTransitions net m =
            mapM_ (assertCnstr <=< checkPrePostTransition) $ transitions net
        where checkPrePostTransition t = do
                incoming <- mElemSum m $ pre net t
                outgoing <- mElemSum m $ post net t
                in1 <- mkEq incoming =<< mkVal (1::Integer)
                out1 <- mkEq outgoing =<< mkVal (1::Integer)
                lhs <- mElem m t
                rhs <- mkAnd [in1, out1]
                mkImplies lhs rhs

checkSubsetTransitions :: [String] -> MModelS -> Z3 ()
checkSubsetTransitions fired m = do
            mapM_ (assertCnstr <=< checkTransition) fired
            fired'sum <- mElemSum m (map prime fired)
            firedsum <- mElemSum m fired
            assertCnstr =<< mkLt fired'sum firedsum
        where checkTransition t = do
                lhs <- mElem m (prime t)
                rhs <- mElem m t
                mkImplies lhs rhs

checkNotEmpty :: [String] -> MModelS -> Z3 ()
checkNotEmpty fired m = do
        lhs <- mElemSum m (map prime fired)
        assertCnstr =<< mkGt lhs =<< mkVal (0::Integer)

checkClosed :: PetriNet -> MModelI -> MModelS -> Z3 ()
checkClosed net ax m =
            mapM_ (assertCnstr <=< checkPlaceClosed) $ places net
        where checkPlaceClosed p = do
                lhs <- mElem m p
                rhs <- mkAnd' =<< mapM checkTransition
                             [(t,t') | t <- pre net p, t' <- post net p,
                                       cElem ax t, cElem ax t' ]
                mkImplies lhs rhs
              checkTransition (t,t') = do
                lhs <- mElem m (prime t)
                rhs <- mElem m (prime t')
                mkImplies lhs rhs

checkTokens :: PetriNet -> MModelS -> Z3 ()
checkTokens net m = do
            initS <- mapM addPlace (linitials net)
            sums <- mkAdd' initS
            assertCnstr =<< mkEq sums =<< mkVal (1::Integer)
        where addPlace (p,x) =
                mkVal x >>= \x' -> mkMul [x', mVal m p]

checkBinary :: MModelS -> Z3 ()
checkBinary m =
            mapM_ (assertCnstr <=< checkBinaryVal) $ mValues m
        where checkBinaryVal x = do
                x0 <- mkEq x =<< mkVal (0::Integer)
                x1 <- mkEq x =<< mkVal (1::Integer)
                mkOr [x0,x1]

checkSComponent :: PetriNet -> [String] -> MModelI -> MModelS -> Z3 ()
checkSComponent net fired ax m = do
        checkPrePostPlaces net m
        checkPrePostTransitions net m
        checkSubsetTransitions fired m
        checkNotEmpty fired m
        checkClosed net ax m
        checkTokens net m
        checkBinary m

checkSComponentSat :: PetriNet -> [String] -> MModelI -> ([String], MModelS -> Z3 ())
checkSComponentSat net fired ax =
        (places net ++ transitions net ++ map prime fired,
         checkSComponent net fired ax)

getSComponentOutIn :: PetriNet -> MModelI -> MModelI -> ([String], [String])
getSComponentOutIn net ax as =
        partition (cElem ax) $ filter (cElem as) (transitions net)


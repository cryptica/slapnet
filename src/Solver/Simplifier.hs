module Solver.Simplifier (
    checkSubsumptionSat
) where

import Data.SBV
import qualified Data.Map as M
import qualified Data.Set as S

import Util
import Solver
import Solver.LivenessInvariant
import PetriNet

checkTransPositive :: SBMap Transition -> S.Set Transition -> SBool
checkTransPositive m ts = bOr $ map (val m) $ S.elems ts

checkTransNegative :: SBMap Transition -> S.Set Transition -> SBool
checkTransNegative m ts = bAnd $ map (bnot . val m) $ S.elems ts

checkCutPositive :: SBMap Transition -> SimpleCut -> SBool
checkCutPositive m (c0, cs) =
        checkTransNegative m c0 &&& bAnd (map (checkTransPositive m) cs)

checkCutNegative :: SBMap Transition -> SimpleCut -> SBool
checkCutNegative m (c0, cs) =
        checkTransPositive m c0 ||| bOr (map (checkTransNegative m) cs)

checkCuts :: SimpleCut -> [SimpleCut] -> SBMap Transition -> SBool
checkCuts c0 cs m = checkCutPositive m c0 &&& bAnd (map (checkCutNegative m) cs)

getSubsumption :: BMap Transition -> [Transition]
getSubsumption m = M.keys (M.filter id m)

checkSubsumptionSat :: PetriNet -> SimpleCut -> [SimpleCut] -> ConstraintProblem Bool [Transition]
checkSubsumptionSat net c0 cs =
        let m = makeVarMap $ transitions net
        in  ("constraint subsumption", "subsumption",
            getNames m,
            \fm -> checkCuts c0 cs (fmap fm m),
            \fm -> getSubsumption (fmap fm m))

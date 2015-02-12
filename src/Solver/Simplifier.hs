module Solver.Simplifier (
     checkSubsumptionSat
    ,SimpleCut
    ,generateCuts
    ,greedySimplify
) where

import Data.SBV
import Data.Maybe
import Control.Monad
import Control.Monad.Identity
import qualified Data.Map as M
import qualified Data.Set as S

import Util
import Options
import Solver
import Solver.TransitionInvariant
import Property
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

checkSubsumptionSat :: SimpleCut -> [SimpleCut] -> ConstraintProblem Bool [Transition]
checkSubsumptionSat c0 cs =
        let m = makeVarMap $ S.elems $ S.unions $ map cutTransitions (c0:cs)
        in  ("constraint subsumption", "subsumption",
            getNames m,
            \fm -> checkCuts c0 cs (fmap fm m),
            \fm -> getSubsumption (fmap fm m))

cutTransitions :: SimpleCut -> S.Set Transition
cutTransitions (c0, cs) = S.unions (c0:cs)

generateCuts :: PetriNet -> Formula Transition -> [Cut] -> OptIO [SimpleCut]
generateCuts net f cuts = do
            simp <- opt optSimpFormula
            let simpFunctions =
                    [ return . simplifyCuts
                    , return . removeImplicants
                    , return . filterInvariantTransitions net
                    , simplifyBySubsumption
                    , greedySimplify net
                    , simplifyBySubsumption
                    ]
            let (otfSimps, afterSimps) = splitAt 2 $ take simp simpFunctions
            let simpFunction = foldl (>=>) return $ reverse afterSimps
            let otfFunction = foldl (>=>) return $ reverse otfSimps
            let cnfCuts = [formulaToCut f] : map cutToSimpleDNFCuts cuts
            dnfCuts <- foldM (combine otfFunction) [(S.empty, [])] cnfCuts
            simpFunction dnfCuts
        where
            combine simpFunction cs1 cs2 =
                simpFunction [ (c1c0 `S.union` c2c0, c1cs ++ c2cs)
                             | (c1c0, c1cs) <- cs1, (c2c0, c2cs) <- cs2 ]

filterInvariantTransitions :: PetriNet -> [SimpleCut] -> [SimpleCut]
filterInvariantTransitions net =
        let ts = S.fromList $ invariantTransitions net
        in  runIdentity . simplifyWithFilter (return . filterTransitions ts) isMoreGeneralCut

filterTransitions :: S.Set Transition -> SimpleCut -> (Bool, SimpleCut)
filterTransitions ts (c0, cs) =
        let c0' = c0 `S.difference` ts
            cs' = filter (S.null . S.intersection ts) cs
            changed = not $ all (S.null . S.intersection ts) cs
        in  (changed, (c0', cs'))

invariantTransitions :: PetriNet -> [Transition]
invariantTransitions net = filter (\t -> lpre net t == lpost net t) $ transitions net

removeImplicants :: [SimpleCut] -> [SimpleCut]
removeImplicants = removeWith isMoreGeneralCut

simplifyCuts :: [SimpleCut] -> [SimpleCut]
simplifyCuts = mapMaybe simplifyCut

simplifyCut :: SimpleCut -> Maybe SimpleCut
simplifyCut (c0, cs) =
        let remove b a = a `S.difference` b
            cs' = removeWith S.isSubsetOf $ map (remove c0) cs
        in  if any S.null cs' then
                Nothing
            else
                Just (c0, cs')

simplifyBySubsumption :: [SimpleCut] -> OptIO [SimpleCut]
simplifyBySubsumption = simplifyBySubsumption' []

simplifyBySubsumption' :: [SimpleCut] -> [SimpleCut] -> OptIO [SimpleCut]
simplifyBySubsumption' acc [] = return $ reverse acc
simplifyBySubsumption' acc (c0:cs) = do
        -- TODO: check with prime implicants
        r <- checkSat $ checkSubsumptionSat c0 (acc ++ cs)
        let acc' = case r of
                    Nothing -> acc
                    Just _ -> c0:acc
        simplifyBySubsumption' acc' cs

simplifyWithFilter :: (Monad m) => (a -> m (Bool, a)) -> (a -> a -> Bool) -> [a] -> m [a]
simplifyWithFilter simp f = simpFilter []
        where
            simpFilter acc [] = return $ reverse acc
            simpFilter acc (x:xs) = do
                (changed, x') <- simp x
                if changed then
                    simpFilter (x' : notFilter x' acc) (notFilter x' xs)
                else
                    simpFilter (x' : acc) xs
            notFilter x = filter (not . f x)

removeWith :: (a -> a -> Bool) -> [a] -> [a]
removeWith f = removeCuts' []
        where
            removeCuts' acc [] = reverse acc
            removeCuts' acc (x:xs) = removeCuts' (x : notFilter x acc) (notFilter x xs)
            notFilter x = filter (not . f x)

-- c1 `isMoreGeneralCut` c2 <=> (c2 => c1)
isMoreGeneralCut :: SimpleCut -> SimpleCut -> Bool
isMoreGeneralCut (c1c0, c1cs) (c2c0, c2cs) =
        c1c0 `S.isSubsetOf` c2c0 && all (\c1 -> any (`S.isSubsetOf` c1) c2cs) c1cs

cutToSimpleDNFCuts :: Cut -> [SimpleCut]
cutToSimpleDNFCuts (ts, u) = (S.empty, [S.fromList u]) : map (\(_, t) -> (S.fromList t, [])) ts

-- TODO: allow formulas with or
formulaToCut :: Formula Transition -> SimpleCut
formulaToCut = transformF
        where
            transformF FTrue = (S.empty, [])
            transformF (p :&: q) =
                let (p0, ps) = transformF p
                    (q0, qs) = transformF q
                in  (p0 `S.union` q0, ps ++ qs)
            transformF (LinearInequation ts Gt (Const 0)) =
                (S.empty, [transformTerm ts])
            transformF (LinearInequation ts Ge (Const 1)) =
                (S.empty, [transformTerm ts])
            transformF (LinearInequation ts Eq (Const 0)) =
                (transformTerm ts, [])
            transformF (LinearInequation ts Le (Const 0)) =
                (transformTerm ts, [])
            transformF (LinearInequation ts Lt (Const 1)) =
                (transformTerm ts, [])
            transformF f =
                error $ "formula not supported for invariant: " ++ show f
            transformTerm (t :+: u) = transformTerm t `S.union` transformTerm u
            transformTerm (Var x) = S.singleton x
            transformTerm t =
                error $ "term not supported for invariant: " ++ show t

checkCut :: PetriNet -> SimpleCut -> OptIO Bool
checkCut net cut = do
        r <- checkSat $ checkTransitionInvariantWithSimpleCutSat net cut
        return $ isNothing r

greedySimplifyCut :: PetriNet -> Bool -> SimpleCut -> SimpleCut-> OptIO (Bool, SimpleCut)
greedySimplifyCut net changed cutAcc@(c0Acc, csAcc) (c0, cs) =
        case (S.null c0, cs) of
            (True, []) -> return (changed, cutAcc)
            (False, _) -> do
                let (c, c0') = S.deleteFindMin c0
                let cut = (c0Acc `S.union` c0', csAcc ++ cs)
                r <- checkCut net cut
                greedySimplifyCut net (r || changed)
                    (if r then cutAcc else (S.insert c c0Acc, csAcc)) (c0', cs)
            (True, c:cs') -> do
                let cut = (c0Acc `S.union` c0, csAcc ++ cs')
                r <- checkCut net cut
                greedySimplifyCut net (r || changed)
                    (if r then cutAcc else (c0Acc, c:csAcc)) (c0, cs')

greedySimplify :: PetriNet -> [SimpleCut] -> OptIO [SimpleCut]
greedySimplify net =
        simplifyWithFilter (greedySimplifyCut net False (S.empty, [])) isMoreGeneralCut


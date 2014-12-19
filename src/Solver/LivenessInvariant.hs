module Solver.LivenessInvariant (
    checkLivenessInvariantSat
  , LivenessInvariant
) where

import Data.SBV
import Data.List (intercalate)
import qualified Data.Map as M

import Util
import Solver
import Property
import PetriNet

data LivenessInvariant =
            RankingFunction (String, [SimpleCut], Vector Place)
          | ComponentCut (String, [SimpleCut], [Trap])

showSimpleCuts :: [SimpleCut] -> Bool -> String
showSimpleCuts cs inv = intercalate " ∧ " (map showTrans cs)
        where showTrans (ts, b) =
                  if b && inv || not b && not inv then
                       intercalate " ∧ " (map (\t -> show t ++ " ∉ σ") ts)
                  else
                       let d = intercalate " ∨ "
                                (map (\t -> show t ++ " ∈ σ") ts)
                       in  if length ts == 1 then d else "(" ++ d ++ ")"

instance Show LivenessInvariant where
        show (RankingFunction (n, cs, xs)) = n ++
                    " [" ++ showSimpleCuts cs True ++ "]: " ++
                    intercalate " + " (map showPlace (items xs))
                where showPlace (p, w) = show w ++ show p
        show (ComponentCut (n, cs, ps)) = n ++
                    " [" ++ showSimpleCuts cs False ++ "]: " ++
                    show ps

type SimpleCut = ([Transition], Bool)
type NamedCut = (String, [(String, SimpleCut)])

placeName :: String -> Place -> String
placeName n p = n ++ "@p" ++ show p

numPref :: String -> [String]
numPref s = map (\i -> s ++ show i) [(1::Integer)..]

generateCuts :: Formula Transition -> [Cut] -> [NamedCut]
generateCuts f cuts =
            zipWith nameCut
                (numPref "@r")
                (foldl combine [formulaToCut f] (map cutToSimpleCuts cuts))
        where
            nameCut n c = (n, numPref "@comp" `zip` c)
            combine curCuts compCut = [x : c | x <- compCut, c <- curCuts]

varNames :: PetriNet -> [NamedCut] -> [String]
varNames net = concatMap cutNames
        where
            cutNames (n, c) =
                [n ++ "@comp0"] ++
                map (placeName n) (places net) ++
                map (\(n', _) -> n ++ n') c

cutToSimpleCuts :: Cut -> [SimpleCut]
cutToSimpleCuts (ts,u) = (u, False) : map (\(_, t) -> (t, True)) ts

formulaToCut :: Formula Transition -> [SimpleCut]
formulaToCut = transformF
        where
            transformF FTrue = []
            transformF (p :&: q) = transformF p ++ transformF q
            transformF (LinearInequation ts Gt (Const 0)) =
                [(transformTerm ts, False)]
            transformF (LinearInequation ts Ge (Const 1)) =
                [(transformTerm ts, False)]
            transformF (LinearInequation ts Eq (Const 0)) =
                [(transformTerm ts, True)]
            transformF (LinearInequation ts Le (Const 0)) =
                [(transformTerm ts, True)]
            transformF (LinearInequation ts Lt (Const 1)) =
                [(transformTerm ts, True)]
            transformF f =
                error $ "formula not supported for invariant: " ++ show f
            transformTerm (t :+: u) = transformTerm t ++ transformTerm u
            transformTerm (Var x) = [x]
            transformTerm t =
                error $ "term not supported for invariant: " ++ show t

checkCut :: PetriNet -> SIMap String -> NamedCut -> SBool
checkCut net m (n, comps) =
            bAnd (map checkTransition (transitions net)) &&&
            val m (n ++ "@comp0") + sum (map addComp2 comps) .> 0 &&&
            bAnd (map (checkNonNegativity . placeName n) (places net)) &&&
            checkNonNegativity (n ++ "@comp0") &&&
            bAnd (map (\(n', _) -> checkNonNegativity (n ++ n')) comps)
        where checkTransition t =
                let incoming = map addPlace $ lpre net t
                    outgoing = map addPlace $ lpost net t
                    zeroComp = val m (n ++ "@comp0")
                    addComp1 (n', (ts, xv)) =
                        if t `elem` ts then
                            (if xv then 1 else (-1)) * val m (n ++ n')
                        else
                            0
                    cutComps = map addComp1 comps
                in  sum outgoing - sum incoming + zeroComp .<= sum cutComps
              addPlace (p,w) = literal w * val m (placeName n p)
              addComp2 (n', (_, w)) = if w then 0 else val m (n ++ n')
              checkNonNegativity x = val m x .>= 0

checkLivenessInvariant :: PetriNet -> [NamedCut] -> SIMap String -> SBool
checkLivenessInvariant net cuts m =
        bAnd (map (checkCut net m) cuts)

checkLivenessInvariantSat :: PetriNet -> Formula Transition -> [Cut] ->
        ConstraintProblem Integer [LivenessInvariant]
checkLivenessInvariantSat net f cuts =
        let namedCuts = generateCuts f cuts
            names = varNames net namedCuts
            myVarMap fvm = M.fromList $ names `zip` fmap fvm names
        in  ("liveness invariant constraints", "liveness invariant",
             names,
             checkLivenessInvariant net namedCuts . myVarMap,
             getLivenessInvariant net cuts namedCuts . myVarMap)

getLivenessInvariant :: PetriNet -> [Cut] -> [NamedCut] -> IMap String ->
        [LivenessInvariant]
getLivenessInvariant net cuts namedCuts y =
            map rankCut namedCuts ++ zipWith compCut (numPref "@cut") cuts
        where rankCut (n, c) = RankingFunction
                (n, map snd c,
                 buildVector (map (\p -> (p, val y (placeName n p))) (places net)))
              compCut n c = ComponentCut
                (n, cutToSimpleCuts c, map fst (fst c))



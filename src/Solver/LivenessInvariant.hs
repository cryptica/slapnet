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
import qualified Data.Set as S

data LivenessInvariant =
            RankingFunction (String, SimpleCut, Vector Place)
          | ComponentCut (String, SimpleCut, [Trap])

showSimpleCuts :: SimpleCut -> Bool -> String
showSimpleCuts cs inv = intercalate " ∧ " (showSimpleCut cs)
        where showSimpleCut (ts0, cs1) =
                if S.null ts0 then
                    map (showTrans inv) cs1
                else
                    showTrans (not inv) ts0 : map (showTrans inv) cs1
              showTrans pos ts =
                  if pos then
                       let d = intercalate " ∨ "
                                (map (\t -> show t ++ " ∈ σ") (S.toList ts))
                       in  if S.size ts == 1 then d else "(" ++ d ++ ")"
                  else
                       intercalate " ∧ " (map (\t -> show t ++ " ∉ σ") (S.toList ts))

instance Show LivenessInvariant where
        show (RankingFunction (n, cs, xs)) = n ++
                    " [" ++ showSimpleCuts cs True ++ "]: " ++
                    intercalate " + " (map showWeighted (items xs))
        show (ComponentCut (n, cs, ps)) = n ++
                    " [" ++ showSimpleCuts cs False ++ "]: " ++
                    show ps

type SimpleCut = (S.Set Transition, [S.Set Transition])
type NamedCut = (String, (S.Set Transition, [(String, S.Set Transition)]))

placeName :: String -> Place -> String
placeName n p = n ++ "@p" ++ show p

generateCuts :: Formula Transition -> [Cut] -> [NamedCut]
generateCuts f cuts =
            zipWith nameCut
                (numPref "@r")
                (foldl combine [formulaToCut f] (map cutToSimpleDNFCuts cuts))
        where
            nameCut n (c0, cs) = (n, (c0, numPref "@comp" `zip` cs))
            combine cs1 cs2 = concat [ combineCuts c1 c2 | c1 <- cs1, c2 <- cs2 ]

combineCuts :: SimpleCut -> SimpleCut -> [SimpleCut]
combineCuts (c1c0, c1cs) (c2c0, c2cs) = [(c1c0 `S.union` c2c0, c1cs ++ c2cs)]

varNames :: PetriNet -> [NamedCut] -> [String]
varNames net = concatMap cutNames
        where
            cutNames (n, (_, c)) =
                [n ++ "@yone"] ++ [n ++ "@comp0"] ++
                map (placeName n) (places net) ++
                map (\(n', _) -> n ++ n') c

cutToSimpleDNFCuts :: Cut -> [SimpleCut]
cutToSimpleDNFCuts (ts, u) = (S.empty, [S.fromList u]) : map (\(_, t) -> (S.fromList t, [])) ts

cutToSimpleCNFCut :: Cut -> SimpleCut
cutToSimpleCNFCut (ts, u) = (S.fromList u, map (\(_, t) -> S.fromList t) ts)

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

checkCut :: PetriNet -> SIMap String -> NamedCut -> SBool
checkCut net m (n, (comp0, comps)) =
            bAnd (map checkTransition (transitions net)) &&&
            val m (n ++ "@yone") + sum (map addComp comps) .> 0 &&&
            bAnd (map (checkNonNegativity . placeName n) (places net)) &&&
            checkNonNegativity (n ++ "@yone") &&&
            checkNonNegativity (n ++ "@comp0") &&&
            bAnd (map (\(n', _) -> checkNonNegativity (n ++ n')) comps)
        where checkTransition t =
                let incoming = map addPlace $ lpre net t
                    outgoing = map addPlace $ lpost net t
                    yone = val m (n ++ "@yone")
                    addCompT (n', ts) = if t `S.member` ts then val m (n ++ n') else 0
                    comp0Val = addCompT ("@comp0", comp0)
                    compsVal = sum $ map addCompT comps
                in  sum outgoing - sum incoming + yone + compsVal .<= comp0Val
              addPlace (p,w) = literal w * val m (placeName n p)
              addComp (n', _) = val m (n ++ n')
              checkNonNegativity x = val m x .>= 0

checkLivenessInvariant :: PetriNet -> [NamedCut] -> SIMap String -> SBool
checkLivenessInvariant net cuts m =
        bAnd (map (checkCut net m) cuts)

-- TODO: split up into many smaller sat problems
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
        where rankCut (n, (c0, cs)) = RankingFunction
                (n, (c0, map snd cs),
                 buildVector (map (\p -> (p, val y (placeName n p))) (places net)))
              compCut n c = ComponentCut
                (n, cutToSimpleCNFCut c, map fst (fst c))



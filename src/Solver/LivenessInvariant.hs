module Solver.LivenessInvariant (
    checkLivenessInvariant
  , checkLivenessInvariantSat
  , generateCuts
  , getLivenessInvariant
  , PlaceVector
  , LivenessInvariant(..)
) where

import Data.SBV
import Data.List (intercalate)

import Solver
import Solver.SComponent
import Property
import PetriNet

type PlaceVector = [(String, Integer)]
newtype LivenessInvariant =
        LivenessInvariant { getInv :: (String, [Cut], PlaceVector) }

instance Show LivenessInvariant where
        show (LivenessInvariant (n, cs, xs)) = n ++ " [" ++
                    intercalate " ∧ " (map showTrans cs) ++ "]: " ++
                    intercalate " + " (map showPlace (filter ((> 0) . snd) xs))
                where showPlace (p, w) = show w ++ p
                      showTrans (ts, b) =
                          if b then
                               intercalate " ∧ " (map (++ " ∉ σ") ts)
                          else
                               let d = intercalate " ∨ " (map (++ " ∈ σ") ts)
                               in  if length ts == 1 then d else "(" ++ d ++ ")"

type Cut = ([String], Bool)
type NamedCut = (String, [(String, Cut)])

generateCuts :: Formula -> [SCompCut] -> [NamedCut]
generateCuts f comps =
            zipWith nameCut
                (numPref "@part")
                (foldl combine [formulaToCut f] comps)
        where
            nameCut n c = (n, numPref "@comp" `zip` c)
            numPref s = map (\i -> s ++ show i) [(1::Integer)..]
            combine cuts compCut = [x : c | x <- compCut, c <- cuts]

varNames :: PetriNet -> [NamedCut] -> [String]
varNames net = concatMap cutNames
        where
            cutNames (n, c) =
                [n ++ "@comp0"] ++
                map (\p -> n ++ "@p" ++ p) (places net) ++
                map (\(n', _) -> n ++ n') c

formulaToCut :: Formula -> SCompCut
formulaToCut = transformConj
        where
            transformConj FTrue = []
            transformConj (p :&: q) = transformConj p ++ transformConj q
            transformConj (Atom lieq) = transformLieq lieq
            transformConj form =
                error $ "formula not supported for invariant: " ++ show form
            transformLieq (LinIneq ts Gt (Const 0)) = [(transformTerm ts, False)]
            transformLieq (LinIneq ts Ge (Const 1)) = [(transformTerm ts, False)]
            transformLieq (LinIneq ts Eq (Const 0)) = [(transformTerm ts, True)]
            transformLieq (LinIneq ts Le (Const 0)) = [(transformTerm ts, True)]
            transformLieq (LinIneq ts Lt (Const 1)) = [(transformTerm ts, True)]
            transformLieq l =
                error $ "linear inequation not supported for invariant: " ++ show l
            transformTerm (t :+: u) = transformTerm t ++ transformTerm u
            transformTerm (Var x) = [x]
            transformTerm t =
                error $ "term not supported for invariant: " ++ show t

checkCut :: PetriNet -> ModelSI -> NamedCut -> SBool
checkCut net m (n, comps) =
            bAnd (map checkTransition (transitions net)) &&&
            mVal m (n ++ "@comp0") + sum (map addComp2 comps) .> 0 &&&
            bAnd (map (\p -> checkNonNegativity (n ++ "@p" ++ p)) (places net)) &&&
            checkNonNegativity (n ++ "@comp0") &&&
            bAnd (map (\(n', _) -> checkNonNegativity (n ++ n')) comps)
        where checkTransition t =
                let incoming = map addPlace $ lpre net t
                    outgoing = map addPlace $ lpost net t
                    zeroComp = mVal m (n ++ "@comp0")
                    addComp1 (n', (ts, xv)) =
                        if t `elem` ts then
                            (if xv then 1 else (-1)) * mVal m (n ++ n')
                        else
                            0
                    cutComps = map addComp1 comps
                in  sum outgoing - sum incoming + zeroComp .<= sum cutComps
              addPlace (p,w) = literal w * mVal m (n ++ "@p" ++ p)
              addComp2 (n', (_, w)) = if w then 0 else mVal m (n ++ n')
              checkNonNegativity x = mVal m x .>= 0

checkLivenessInvariant :: PetriNet -> [NamedCut] ->
        ModelSI -> SBool
checkLivenessInvariant net cuts m =
        bAnd (map (checkCut net m) cuts)

checkLivenessInvariantSat :: PetriNet -> [NamedCut] ->
        ([String], ModelSI -> SBool)
checkLivenessInvariantSat net cuts =
        (varNames net cuts, checkLivenessInvariant net cuts)

getLivenessInvariant :: PetriNet -> [NamedCut] -> ModelI -> [LivenessInvariant]
getLivenessInvariant net cuts as = map lookupCut cuts
        where lookupCut (n, c) = LivenessInvariant
               (n, map snd c, map (\p -> (p, mVal as (n ++ "@p" ++ p))) (places net))



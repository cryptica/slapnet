module Solver.LivenessInvariant (
    checkLivenessInvariant
  , checkLivenessInvariantSat
  , getLivenessInvariant
  , PlaceVector
  , LivenessInvariant
) where

import Data.SBV

import Solver
import Solver.SComponent
import PetriNet

type PlaceVector = [(String, Integer)]
type LivenessInvariant = [PlaceVector]

type Cut = ([String], Bool)
type NamedCut = (String, [(String, Cut)])

nameCuts :: [[Cut]] -> [NamedCut]
nameCuts cuts =
            map (\(n, c) -> (n, numPref "@comp" `zip` c)) $
                numPref "@cut" `zip` cuts
        where
            numPref s = map (\i -> s ++ show i) [(1::Integer)..]

varNames :: PetriNet -> [NamedCut] -> [String]
varNames net = concatMap cutNames
        where
            cutNames (n, c) =
                [n ++ "@comp0"] ++
                map (\p -> n ++ "@p" ++ p) (places net) ++
                map (\(n', _) -> n ++ n') c

foldComps :: [SCompCut] -> [[Cut]]
foldComps = foldl combine [[]]
        where
            combine cuts (t1, t2, u) =
                concatMap
                    (\c -> [(t1, True) : c, (t2, True) : c, (u, False) : c]) cuts

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

checkLivenessInvariantSat :: PetriNet -> [SCompCut] ->
        ([String], ModelSI -> SBool)
checkLivenessInvariantSat net comps =
        let cuts = foldComps comps
            namedCuts = nameCuts cuts
            names = varNames net namedCuts
        in  (names, checkLivenessInvariant net namedCuts)

getLivenessInvariant :: PetriNet -> [SCompCut] -> ModelI -> LivenessInvariant
getLivenessInvariant net ax as = undefined

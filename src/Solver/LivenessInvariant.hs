module Solver.LivenessInvariant (
    checkLivenessInvariantSat
  , LivenessInvariant
  , generateCuts
  , simplifyCuts
  , cutToLivenessInvariant
  , SimpleCut
) where

import Data.SBV
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

import Util
import Solver
import Property
import PetriNet

data LivenessInvariant =
            RankingFunction (SimpleCut, Vector Place)
          | ComponentCut (SimpleCut, [Trap])

showSimpleCuts :: SimpleCut -> String
showSimpleCuts cs = intercalate " ∧ " (showSimpleCut cs)
        where showSimpleCut (ts0, cs1) =
                if S.null ts0 then
                    map (showTrans True) cs1
                else
                    showTrans False ts0 : map (showTrans True) cs1
              showTrans pos ts =
                  if pos then
                       let d = intercalate " ∨ "
                                (map (\t -> show t ++ " ∈ σ") (S.toList ts))
                       in  if S.size ts == 1 then d else "(" ++ d ++ ")"
                  else
                       intercalate " ∧ " (map (\t -> show t ++ " ∉ σ") (S.toList ts))

instance Show LivenessInvariant where
        show (RankingFunction (cs, xs)) =
                    "[" ++ showSimpleCuts cs ++ "]: " ++
                    intercalate " + " (map showWeighted (items xs))
        show (ComponentCut (cs, ps)) =
                    "[" ++ showSimpleCuts cs ++ "]: " ++
                    show ps

type SimpleCut = (S.Set Transition, [S.Set Transition])
type NamedCut = (S.Set Transition, [(String, S.Set Transition)])

placeName :: Place -> String
placeName p = "@p" ++ show p

generateCuts :: Formula Transition -> [Cut] -> [SimpleCut]
generateCuts f cuts =
            foldl combine [formulaToCut f] (map cutToSimpleDNFCuts cuts)
        where
            combine cs1 cs2 = [ (c1c0 `S.union` c2c0, c1cs ++ c2cs)
                              | (c1c0, c1cs) <- cs1, (c2c0, c2cs) <- cs2 ]

simplifyCuts :: [SimpleCut] -> [SimpleCut]
simplifyCuts = removeWith isMoreGeneralCut . concatMap simplifyCut

simplifyCut :: SimpleCut -> [SimpleCut]
simplifyCut (c0, cs) =
        let remove b a = a `S.difference` b
            cs' = removeWith S.isSubsetOf $ map (remove c0) cs
        in  if any S.null cs' then
                []
            else
                [(c0, cs')]

nameCut :: SimpleCut -> NamedCut
nameCut (c0, cs) = (c0, numPref "@comp" `zip` cs)

removeWith :: (a -> a -> Bool) -> [a] -> [a]
removeWith f = removeCuts' []
        where
            removeCuts' acc [] = reverse acc
            removeCuts' acc (x:xs) = removeCuts' (x : cutFilter x acc) (cutFilter x xs)
            cutFilter cut = filter (not . f cut)

isMoreGeneralCut :: SimpleCut -> SimpleCut -> Bool
isMoreGeneralCut (c1c0, c1cs) (c2c0, c2cs) =
        c1c0 `S.isSubsetOf` c2c0 && all (\c1 -> any (`S.isSubsetOf` c1) c2cs) c1cs

cutNames :: PetriNet -> NamedCut -> [String]
cutNames net (_, c) =
        ["@yone", "@comp0"] ++
        map placeName (places net) ++
        map fst c

cutToSimpleDNFCuts :: Cut -> [SimpleCut]
cutToSimpleDNFCuts (ts, u) = (S.empty, [S.fromList u]) : map (\(_, t) -> (S.fromList t, [])) ts

cutToSimpleCNFCut :: Cut -> SimpleCut
cutToSimpleCNFCut (ts, u) = (S.fromList u, map (\(_, t) -> S.fromList t) ts)

toSimpleCut :: NamedCut -> SimpleCut
toSimpleCut (c0, ncs) = (c0, map snd ncs)

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

checkLivenessInvariant :: PetriNet -> NamedCut -> SIMap String -> SBool
checkLivenessInvariant net (comp0, comps) m =
            bAnd (map checkTransition (transitions net)) &&&
            val m "@yone" + sum (map addComp comps) .> 0 &&&
            bAnd (map (checkNonNegativity . placeName) (places net)) &&&
            checkNonNegativity "@yone" &&&
            checkNonNegativity "@comp0" &&&
            bAnd (map (\(n, _) -> checkNonNegativity n) comps)
        where checkTransition t =
                let incoming = map addPlace $ lpre net t
                    outgoing = map addPlace $ lpost net t
                    yone = val m "@yone"
                    addCompT (n, ts) = if t `S.member` ts then val m n else 0
                    comp0Val = addCompT ("@comp0", comp0)
                    compsVal = sum $ map addCompT comps
                in  sum outgoing - sum incoming + yone + compsVal .<= comp0Val
              addPlace (p,w) = literal w * val m (placeName p)
              addComp (n, _) = val m n
              checkNonNegativity x = val m x .>= 0

checkLivenessInvariantSat :: PetriNet -> SimpleCut -> ConstraintProblem Integer LivenessInvariant
checkLivenessInvariantSat net cut =
        let namedCut = nameCut cut
            names = cutNames net namedCut
            myVarMap fvm = M.fromList $ names `zip` fmap fvm names
        in  ("liveness invariant constraints", "liveness invariant",
             names,
             checkLivenessInvariant net namedCut . myVarMap,
             getLivenessInvariant net namedCut . myVarMap)

cutToLivenessInvariant :: Cut -> LivenessInvariant
cutToLivenessInvariant c = ComponentCut (cutToSimpleCNFCut c, map fst (fst c))

getLivenessInvariant :: PetriNet -> NamedCut -> IMap String -> LivenessInvariant
getLivenessInvariant net cut y =
        RankingFunction
                (toSimpleCut cut,
                 buildVector (map (\p -> (p, val y (placeName p))) (places net)))

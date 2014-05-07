module Solver
    (checkSat,checkPropertyConstraintsSat,checkTrapConstraintsSat)
where

import Data.SBV
import qualified Data.Map as M

import PetriNet
import Property

type ModelSI = M.Map String SInteger
type ModelSB = M.Map String SBool
type ModelI = M.Map String Integer
type ModelB = M.Map String Bool
type ModelLI = [(String, Integer)]
type ModelLB = [(String, Bool)]

evaluateTerm :: ModelSI -> Term -> SInteger
evaluateTerm m (Term xs) = sum $ map evaluateLinAtom xs
        where evaluateLinAtom (Var c x) = literal c * m M.! x
              evaluateLinAtom (Const c) = literal c

opToFunction :: Op -> SInteger -> SInteger -> SBool
opToFunction Gt = (.>)
opToFunction Ge = (.>=)
opToFunction Eq = (.==)
opToFunction Le = (.<=)
opToFunction Lt = (.<)

evaluateLinIneq :: ModelSI -> LinearInequation -> SBool
evaluateLinIneq m (LinIneq lhs op rhs) =
        opToFunction op (evaluateTerm m lhs) (evaluateTerm m rhs)

evaluateFormula :: ModelSI -> Formula -> SBool
evaluateFormula m (Atom a) = evaluateLinIneq m a
evaluateFormula m (Neg p) = bnot $ evaluateFormula m p
evaluateFormula m (p :&: q) = evaluateFormula m p &&& evaluateFormula m q
evaluateFormula m (p :|: q) = evaluateFormula m p ||| evaluateFormula m q

checkNonnegativityConstraints :: ModelSI -> PetriNet -> SBool
checkNonnegativityConstraints m net =
            bAnd $ map checkPT $ places net ++ transitions net
        where checkPT x = (m M.! x) .>= 0

checkPlaceEquation :: ModelSI -> PetriNet -> String -> SBool
checkPlaceEquation m net p =
            let incoming = map addTransition $ lpre net p
                outgoing = map addTransition $ lpost net p
                pinit = literal $ initial net p
            in  pinit + sum incoming - sum outgoing .== (m M.! p)
        where addTransition (t,w) = literal w * (m M.! t)

checkStateConstraints :: ModelSI -> PetriNet -> SBool
checkStateConstraints m net =
        bAnd $ map (checkPlaceEquation m net) $ places net

checkTransitionEquation :: ModelSI -> PetriNet -> String -> SBool
checkTransitionEquation m net t =
            let incoming = map addPlace $ lpre net t
                outgoing = map addPlace $ lpost net t
            in  sum outgoing - sum incoming .>= 0
        where addPlace (p,w) = literal w * (m M.! p)

checkTInvariantConstraints :: ModelSI -> PetriNet -> SBool
checkTInvariantConstraints m net =
        bAnd $ map (checkTransitionEquation m net) $ transitions net

checkTrapConstraints :: ModelSB -> PetriNet -> SBool
checkTrapConstraints m net =
            bAnd $ map trapConstraint $ transitions net
        where trapConstraint t =
                bOr (map (m M.!) $ pre net t) ==> bOr (map (m M.!) $ post net t)

checkTrapMarked :: ModelSB -> PetriNet -> SBool
checkTrapMarked m net =
        let marked = map fst $ filter (( > 0) . snd) $ (initials net)
        in  bOr $ map (m M.!) marked

checkTrapUnassigned :: ModelSB -> ModelI -> SBool
checkTrapUnassigned mt ma =
        let assigned = map fst $ filter (( > 0) . snd) $ M.toList ma
        in  bAnd $ map (bnot . (mt M.!)) assigned

checkAllTrapConstraints :: ModelSB -> ModelI -> PetriNet -> SBool
checkAllTrapConstraints mt ma net =
        let tc = checkTrapConstraints mt net
            tm = checkTrapMarked mt net
            tu = checkTrapUnassigned mt ma
        in  tc &&& tm &&& tu

checkPropertyConstraints :: ModelSI -> PetriNet -> Property -> SBool
checkPropertyConstraints m net p =
        let netConstraints = case ptype p of
                  Safety -> checkStateConstraints m net
                  Liveness -> checkTInvariantConstraints m net
            nonnegativityConstraint = checkNonnegativityConstraints m net
            propertyConstraint = evaluateFormula m (pformula p)
        in  netConstraints &&& nonnegativityConstraint &&& propertyConstraint

symConstraints :: SymWord a => [String] -> ([(String, SBV a)] -> SBool) ->
        Symbolic SBool
symConstraints vars constraint = do
        syms <- mapM exists vars
        return $ constraint (vars `zip` syms)

rebuildModel :: SymWord a => [String] -> Either String (Bool, [a]) -> Maybe [(String,a)]
rebuildModel _ (Left _) = Nothing
rebuildModel _ (Right (True, _)) = error "Prover returned unknown"
rebuildModel vars (Right (False, m)) = Just $ vars `zip` m

checkSat :: (SatModel a, SymWord a) => [String] ->
        ([(String, SBV a)] -> SBool) -> IO (Maybe [(String,a)])
checkSat vars constraint = do
        result <- sat $ symConstraints vars constraint
        return $ rebuildModel vars $ getModel result

checkPropertyConstraintsSat :: PetriNet -> Property -> IO (Maybe ModelLI)
checkPropertyConstraintsSat net p =
        let vars = places net ++ transitions net
            cons m = checkPropertyConstraints (M.fromList m) net p
        in  checkSat vars cons

checkTrapConstraintsSat :: PetriNet -> ModelI -> IO (Maybe ModelLB)
checkTrapConstraintsSat net ma =
        let vars = transitions net
            cons m = checkAllTrapConstraints (M.fromList m) ma net
        in  checkSat vars cons


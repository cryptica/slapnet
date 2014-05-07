module Solver
    (checkSat)
where

import Data.SBV
import qualified Data.Map as M
import Control.Monad (liftM,liftM2)

import PetriNet
import Property

type Model = M.Map String SInteger

evaluateTerm :: Model -> Term -> Symbolic SInteger
evaluateTerm m (Term xs) = liftM sum $ mapM evaluateLinAtom xs
        where evaluateLinAtom (Var c x) = return $ literal c * m M.! x
              evaluateLinAtom (Const c) = return $ literal c

opToFunction :: Op -> SInteger -> SInteger -> SBool
opToFunction Gt = (.>)
opToFunction Ge = (.>=)
opToFunction Eq = (.==)
opToFunction Le = (.<=)
opToFunction Lt = (.<)

evaluateLinIneq :: Model -> LinearInequation -> Symbolic SBool
evaluateLinIneq m (LinIneq lhs op rhs) =
        liftM2 (opToFunction op) (evaluateTerm m lhs) (evaluateTerm m rhs)

evaluateFormula :: Model -> Formula -> Symbolic SBool
evaluateFormula m (Atom a) = evaluateLinIneq m a
evaluateFormula m (Neg p) = liftM bnot $ evaluateFormula m p
evaluateFormula m (p :&: q) = do
        r1 <- evaluateFormula m p
        r2 <- evaluateFormula m q
        return $ r1 &&& r2
evaluateFormula m (p :|: q) = do
        r1 <- evaluateFormula m p
        r2 <- evaluateFormula m q
        return $ r1 ||| r2

checkNonnegativityConstraints :: Model -> PetriNet -> Symbolic SBool
checkNonnegativityConstraints m net = do
            pts <- mapM checkPT $ places net ++ transitions net
            return $ bAnd pts
        where checkPT x = return $ (m M.! x) .>= 0

checkPlaceEquation :: Model -> PetriNet -> String -> Symbolic SBool
checkPlaceEquation m net p = do
            incoming <- mapM addTransition $ lpre net p
            outgoing <- mapM addTransition $ lpost net p
            let pinit = literal $ initial net p
            return $ pinit + sum incoming - sum outgoing .== (m M.! p)
        where addTransition (t,w) = return $ literal w * (m M.! t)

checkStateConstraints :: Model -> PetriNet -> Symbolic SBool
checkStateConstraints m net = do
        pEquations <- mapM (checkPlaceEquation m net) $ places net
        return $ bAnd pEquations

checkTransitionEquation :: Model -> PetriNet -> String -> Symbolic SBool
checkTransitionEquation m net t = do
            incoming <- mapM addPlace $ lpre net t
            outgoing <- mapM addPlace $ lpost net t
            return $ sum outgoing - sum incoming .>= 0
        where addPlace (p,w) = return $ literal w * (m M.! p)

checkTInvariantConstraints :: Model -> PetriNet -> Symbolic SBool
checkTInvariantConstraints m net = do
        tEquations <- mapM (checkTransitionEquation m net) $ transitions net
        return $ bAnd tEquations

buildSymbolicModel :: PetriNet -> Symbolic Model
buildSymbolicModel net = do
        let vars = places net ++ transitions net
        syms <- mapM exists vars
        return $ M.fromList (vars `zip` syms)

checkConstraints :: PetriNet -> Property -> Symbolic SBool
checkConstraints net p = do
        model <- buildSymbolicModel net
        r1 <- case ptype p of
                  Safety -> checkStateConstraints model net
                  Liveness -> checkTInvariantConstraints model net
        r2 <- checkNonnegativityConstraints model net
        r3 <- evaluateFormula model (pformula p)
        return $ r1 &&& r2 &&& r3

checkSat :: PetriNet -> Property -> IO (Maybe (M.Map String Integer))
checkSat net p = do
        (SatResult result) <- sat $ checkConstraints net p
        return $ case result of
            Unsatisfiable _-> Nothing
            Satisfiable _ _ -> Just $ M.map fromCW $ getModelDictionary result
            Unknown _ _ -> error "Prover returned unknown"
            ProofError _ xs -> error $ unlines $ "Prover error:"  : xs
            TimeOut _ -> error "Prover timeout"


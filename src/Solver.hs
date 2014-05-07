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

checkPlaceEquation :: Model -> PetriNet -> String -> Symbolic SBool
checkPlaceEquation m net p = do
            incoming <- mapM addTransition $ lpre net p
            outgoing <- mapM addTransition $ lpost net p
            let pinit = literal $ initial net p
            return $ pinit + sum incoming - sum outgoing .== (m M.! p)
        where addTransition (t,w) = return $ literal w * (m M.! t)

checkStateConstraints :: Model -> PetriNet -> Symbolic SBool
checkStateConstraints m net = do
        placeEquations <- mapM (checkPlaceEquation m net) $ places net
        return $ bAnd placeEquations

buildModel :: PetriNet -> Symbolic Model
buildModel net = do
        let vars = places net ++ transitions net
        syms <- mapM exists vars
        return $ M.fromList (vars `zip` syms)

checkConstraints :: PetriNet -> Property -> Symbolic SBool
checkConstraints net p = do
        model <- buildModel net
        r1 <- checkStateConstraints model net
        r2 <- evaluateFormula model (pformula p)
        return $ r1 &&& r2

checkSat :: PetriNet -> Property -> IO Bool
checkSat net p = do
        (SatResult result) <- sat $ checkConstraints net p
        case result of
            Unsatisfiable _ -> return False
            Satisfiable _ _ -> return True
            Unknown _ _ -> error "Prover returned unknown"
            ProofError _ xs -> error $ unlines $ "Prover error:" : xs
            TimeOut _ -> error "Prover timeout"


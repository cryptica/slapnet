module Solver.Formula
    (evaluateFormula)
where

import Data.SBV
import qualified Data.Map as M

import Property
import Solver

evaluateTerm :: Term -> ModelSI -> SInteger
evaluateTerm (Term xs) m = sum $ map evaluateLinAtom xs
        where evaluateLinAtom (Var c x) = literal c * m M.! x
              evaluateLinAtom (Const c) = literal c

opToFunction :: Op -> SInteger -> SInteger -> SBool
opToFunction Gt = (.>)
opToFunction Ge = (.>=)
opToFunction Eq = (.==)
opToFunction Le = (.<=)
opToFunction Lt = (.<)

evaluateLinIneq :: LinearInequation -> ModelSI -> SBool
evaluateLinIneq (LinIneq lhs op rhs) m =
        opToFunction op (evaluateTerm lhs m) (evaluateTerm rhs m)

evaluateFormula :: Formula -> ModelSI -> SBool
evaluateFormula (Atom a) m = evaluateLinIneq a m
evaluateFormula (Neg p) m = bnot $ evaluateFormula p m
evaluateFormula (p :&: q) m = evaluateFormula p m &&& evaluateFormula q m
evaluateFormula (p :|: q) m = evaluateFormula p m ||| evaluateFormula q m

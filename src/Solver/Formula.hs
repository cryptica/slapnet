module Solver.Formula
    (evaluateFormula)
where

import Data.SBV

import Property
import Solver

evaluateTerm :: Term -> ModelSI -> SInteger
evaluateTerm (Var x) m = mVal m x
evaluateTerm (Const c) _ = literal c
evaluateTerm (Minus t) m = - evaluateTerm t m
evaluateTerm (t :+: u) m = evaluateTerm t m + evaluateTerm u m
evaluateTerm (t :-: u) m = evaluateTerm t m - evaluateTerm u m
evaluateTerm (t :*: u) m = evaluateTerm t m * evaluateTerm u m

opToFunction :: Op -> SInteger -> SInteger -> SBool
opToFunction Gt = (.>)
opToFunction Ge = (.>=)
opToFunction Eq = (.==)
opToFunction Ne = (./=)
opToFunction Le = (.<=)
opToFunction Lt = (.<)

evaluateLinIneq :: LinearInequation -> ModelSI -> SBool
evaluateLinIneq (LinIneq lhs op rhs) m =
        opToFunction op (evaluateTerm lhs m) (evaluateTerm rhs m)

evaluateFormula :: Formula -> ModelSI -> SBool
evaluateFormula FTrue _ = true
evaluateFormula FFalse _ = false
evaluateFormula (Atom a) m = evaluateLinIneq a m
evaluateFormula (Neg p) m = bnot $ evaluateFormula p m
evaluateFormula (p :&: q) m = evaluateFormula p m &&& evaluateFormula q m
evaluateFormula (p :|: q) m = evaluateFormula p m ||| evaluateFormula q m

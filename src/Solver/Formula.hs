module Solver.Formula
    (evaluateFormula)
where

import Z3.Monad

import Property
import Solver

evaluateTerm :: Term -> MModelS -> Z3 AST
evaluateTerm (Var x) m = return $ mVal m x
evaluateTerm (Const c) _ = mkInt c
evaluateTerm (Minus t) m = mkUnaryMinus =<< evaluateTerm t m
evaluateTerm (t :+: u) m = evalBinaryTerm m mkAdd t u
evaluateTerm (t :-: u) m = evalBinaryTerm m mkSub t u
evaluateTerm (t :*: u) m = evalBinaryTerm m mkMul t u

evalBinaryTerm :: MModelS -> ([AST] -> Z3 AST) -> Term -> Term -> Z3 AST
evalBinaryTerm m op t u = do
        t' <- evaluateTerm t m
        u' <- evaluateTerm u m
        op [t',u']

opToFunction :: Op -> AST -> AST -> Z3 AST
opToFunction Gt = mkGt
opToFunction Ge = mkGe
opToFunction Eq = mkEq
opToFunction Ne = \a b -> mkNot =<< mkEq a b
opToFunction Le = mkLe
opToFunction Lt = mkLt

evaluateLinIneq :: LinearInequation -> MModelS -> Z3 AST
evaluateLinIneq (LinIneq lhs op rhs) m = do
        lhs' <- evaluateTerm lhs m
        rhs' <- evaluateTerm rhs m
        opToFunction op lhs' rhs'

evaluateFormula :: Formula -> MModelS -> Z3 AST
evaluateFormula FTrue _ = mkTrue
evaluateFormula FFalse _ = mkFalse
evaluateFormula (Atom a) m = evaluateLinIneq a m
evaluateFormula (Neg p) m = mkNot =<< evaluateFormula p m
evaluateFormula (p :&: q) m = do
        p' <- evaluateFormula p m
        q' <- evaluateFormula q m
        mkAnd [p',q']
evaluateFormula (p :|: q) m = do
        p' <- evaluateFormula p m
        q' <- evaluateFormula q m
        mkOr [p',q']

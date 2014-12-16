module Solver.Formula
    (evaluateFormula)
where

import Data.SBV

import Property
import Solver

evaluateTerm :: (Ord a) => Term a -> VarMap a -> ModelReader SInteger SInteger
evaluateTerm (Var x) m = val m x
evaluateTerm (Const c) _ = return $ literal c
evaluateTerm (Minus t) m = do
        tVal <- evaluateTerm t m
        return (- tVal)
evaluateTerm (t :+: u) m = do
        tVal <- evaluateTerm t m
        uVal <- evaluateTerm u m
        return $ tVal + uVal
evaluateTerm (t :-: u) m = do
        tVal <- evaluateTerm t m
        uVal <- evaluateTerm u m
        return $ tVal - uVal
evaluateTerm (t :*: u) m = do
        tVal <- evaluateTerm t m
        uVal <- evaluateTerm u m
        return $ tVal * uVal

opToFunction :: Op -> SInteger -> SInteger -> SBool
opToFunction Gt = (.>)
opToFunction Ge = (.>=)
opToFunction Eq = (.==)
opToFunction Ne = (./=)
opToFunction Le = (.<=)
opToFunction Lt = (.<)

evaluateFormula :: (Ord a) => Formula a -> VarMap a -> IntConstraint
evaluateFormula FTrue _ = return true
evaluateFormula FFalse _ = return false
evaluateFormula (LinearInequation lhs op rhs) m = do
        lhsVal <- evaluateTerm lhs m
        rhsVal <- evaluateTerm rhs m
        return $ opToFunction op lhsVal rhsVal
evaluateFormula (Neg p) m = do
        pVal <- evaluateFormula p m
        return $ bnot pVal
evaluateFormula (p :&: q) m = do
        pVal <- evaluateFormula p m
        qVal <- evaluateFormula q m
        return $ pVal &&& qVal
evaluateFormula (p :|: q) m = do
        pVal <- evaluateFormula p m
        qVal <- evaluateFormula q m
        return $ pVal ||| qVal

module Printer.SARA
    (printProperty)
where

import Data.List (intercalate)

import Printer
import PetriNet
import Property

printSimpleTerm :: Integer -> Term -> String
printSimpleTerm fac (Var x) = if fac == 1 then x else show fac ++ x
printSimpleTerm fac (Const c) = show (fac*c)
printSimpleTerm fac (Const c :*: t) = printSimpleTerm (fac*c) t
printSimpleTerm fac (t :*: Const c) = printSimpleTerm (fac*c) t
printSimpleTerm fac (Minus t) = printSimpleTerm (-fac) t
printSimpleTerm _ t = error $ "term not supported for sara: " ++ show t

printTerm :: Term -> String
printTerm (t :+: u) = printTerm t ++ "+" ++ printSimpleTerm 1 u
printTerm (t :-: u) = printTerm t ++ "+" ++ printSimpleTerm (-1) u
printTerm t = printSimpleTerm 1 t

printOp :: Op -> String
printOp Ge = ">"
printOp Eq = ":"
printOp Le = "<"
printOp op = error $ "operand not supported for sara: " ++ show op

printLinIneq :: LinearInequation -> String
printLinIneq (LinIneq lhs op (Const c)) = printTerm lhs ++ printOp op ++ show c
printLinIneq l = error $ "linear inequation not supported for sara: " ++ show l

printFormula :: Formula -> String
printFormula (Atom a) = printLinIneq a
printFormula (Neg _) = error "negation not supported for sara"
printFormula (p :&: q) = printFormula p ++ "," ++ printFormula q
printFormula f = error $ "formula not supported for sara: " ++ show f

printProperty :: String -> PetriNet -> Property -> String
printProperty filename net (Property propname Safety f) =
        "PROBLEM " ++ validateId propname ++ ":\n" ++
        "GOAL REACHABILITY;\n" ++
        "FILE " ++ reverse (takeWhile (/='/') (reverse filename)) ++
            " TYPE LOLA;\n" ++
        "INITIAL " ++ intercalate ","
            (map (\(p,i) -> p ++ ":" ++ show i) (initials net)) ++ ";\n" ++
        "FINAL COVER;\n" ++
        "CONSTRAINTS " ++ printFormula f ++ ";"
printProperty _ _ (Property _ Liveness _) =
        error "liveness property not supported for sara"

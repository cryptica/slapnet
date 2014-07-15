module Printer
    (printNet,printProperty)
where

import Data.List (intercalate)

import PetriNet
import Property

printNet :: PetriNet -> String
printNet net =
        let showWeight (p,x) = p ++ ":" ++ show x
            ps = "PLACE " ++ intercalate "," (places net) ++ ";\n"
            is = "MARKING " ++ intercalate ","
                    (map showWeight (initials net)) ++ ";\n"
            makeTransition t =
                let (preT,postT) = context net t
                    preS = "CONSUME " ++ intercalate ","
                                (map showWeight preT) ++ ";\n"
                    postS = "PRODUCE " ++ intercalate ","
                                (map showWeight postT) ++ ";\n"
                in  "TRANSITION " ++ t ++ "\n" ++ preS ++ postS
            ts = map makeTransition (transitions net)
        in  unlines (ps:is:ts)

printTerm :: Term -> String
printTerm (Var x) = x
printTerm (Const c) = show c
printTerm (Minus t) = "-" ++ printTerm t
printTerm (t :+: u) = "(" ++ printTerm t ++ " + " ++ printTerm u ++ ")"
printTerm (t :-: u) = "(" ++ printTerm t ++ " - " ++ printTerm u ++ ")"
printTerm (t :*: u) = printTerm t ++ " * " ++ printTerm u

printOp :: Op -> String
printOp Gt = " > "
printOp Ge = " >= "
printOp Eq = " = "
printOp Ne = " != "
printOp Le = " <= "
printOp Lt = " < "

printLinIneq :: LinearInequation -> String
printLinIneq (LinIneq lhs op rhs) = printTerm lhs ++ printOp op ++ printTerm rhs

printFormula :: Formula -> String
printFormula FTrue = "TRUE"
printFormula FFalse = "FALSE"
printFormula (Atom a) = printLinIneq a
printFormula (Neg p) = "NOT " ++ "(" ++ printFormula p ++ ")"
printFormula (p :&: q) = "(" ++ printFormula p ++ " AND " ++ printFormula q ++ ")"
printFormula (p :|: q) = "(" ++ printFormula p ++ " OR " ++ printFormula q ++ ")"

printProperty :: Property -> String
printProperty (Property _ Safety f) = "EF (" ++ printFormula f ++ ")\n"
printProperty (Property _ Liveness _) = error "Liveness property not supported"

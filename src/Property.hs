{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Property
    (Property(..),
     showPropertyName,
     rename,
     PropertyType(..),
     PropertyContent(..),
     Formula(..),
     LinearInequation(..),
     Op(..),
     Term(..),
     PropResult(..),
     resultAnd,
     resultOr,
     resultNot,
     resultsAnd,
     resultsOr)
where

import Structure

data Term = Var String
          | Const Integer
          | Minus Term
          | Term :+: Term
          | Term :-: Term
          | Term :*: Term
          deriving (Eq)

instance Show Term where
        show (Var x) = x
        show (Const c) = show c
        show (Minus t) = "-" ++ show t
        show (t :+: u) = "(" ++ show t ++ " + " ++ show u ++ ")"
        show (t :-: u) = "(" ++ show t ++ " - " ++ show u ++ ")"
        show (t :*: u) = show t ++ " * " ++ show u

renameTerm :: (String -> String) -> Term -> Term
renameTerm f (Var x) = Var (f x)
renameTerm _ (Const c) = Const c
renameTerm f (Minus t) = Minus (renameTerm f t)
renameTerm f (t :+: u) = renameTerm f t :+: renameTerm f u
renameTerm f (t :-: u) = renameTerm f t :-: renameTerm f u
renameTerm f (t :*: u) = renameTerm f t :*: renameTerm f u

data Op = Gt | Ge | Eq | Ne | Le | Lt deriving (Eq)

instance Show Op where
        show Gt = ">"
        show Ge = "≥"
        show Eq = "="
        show Ne = "≠"
        show Le = "≤"
        show Lt = "<"

-- TODO: merge LinIneq constructor into Formula
data LinearInequation = LinIneq Term Op Term deriving (Eq)

instance Show LinearInequation where
        show (LinIneq lhs op rhs) = show lhs ++ " " ++ show op ++ " " ++ show rhs

renameLinIneq :: (String -> String) -> LinearInequation -> LinearInequation
renameLinIneq f (LinIneq lhs op rhs) =
        LinIneq (renameTerm f lhs) op (renameTerm f rhs)

data Formula = FTrue | FFalse
             | Atom LinearInequation
             | Neg Formula
             | Formula :&: Formula
             | Formula :|: Formula
             deriving (Eq)

infixr 3 :&:
infixr 2 :|:

instance Show Formula where
        show FTrue = "true"
        show FFalse = "false"
        show (Atom a) = show a
        show (Neg p) = "¬" ++ "(" ++ show p ++ ")"
        show (p :&: q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
        show (p :|: q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"

renameFormula :: (String -> String) -> Formula -> Formula
renameFormula _ FTrue = FTrue
renameFormula _ FFalse = FFalse
renameFormula f (Atom a) = Atom (renameLinIneq f a)
renameFormula f (Neg p) = Neg (renameFormula f p)
renameFormula f (p :&: q) = renameFormula f p :&: renameFormula f q
renameFormula f (p :|: q) = renameFormula f p :|: renameFormula f q

-- TODO: add functions to transform formula to CNF/DNF

data PropertyType = SafetyType
                  | LivenessType
                  | StructuralType

data PropertyContent = Safety Formula
                  | Liveness Formula
                  | Structural Structure

showPropertyType :: PropertyContent -> String
showPropertyType (Safety _) = "safety"
showPropertyType (Liveness _) = "liveness"
showPropertyType (Structural _) = "structural"

showPropertyContent :: PropertyContent -> String
showPropertyContent (Safety f) = show f
showPropertyContent (Liveness f) = show f
showPropertyContent (Structural s) = show s

data Property = Property {
        pname :: String,
        pcont :: PropertyContent
}

instance Show Property where
        show p =
            showPropertyName p ++
            " { " ++ showPropertyContent (pcont p) ++ " }"

rename :: (String -> String) -> Property -> Property
rename f (Property pn (Safety pf)) = Property pn (Safety (renameFormula f pf))
rename f (Property pn (Liveness pf)) = Property pn (Liveness (renameFormula f pf))
rename _ (Property pn (Structural pc)) = Property pn (Structural pc)

showPropertyName :: Property -> String
showPropertyName p = showPropertyType (pcont p) ++ " property" ++
               (if null (pname p) then "" else " " ++ show (pname p))

data PropResult = Satisfied | Unsatisfied | Unknown deriving (Show,Read,Eq)

resultAnd :: PropResult -> PropResult -> PropResult
resultAnd Satisfied x = x
resultAnd Unsatisfied _ = Unsatisfied
resultAnd _ Unsatisfied = Unsatisfied
resultAnd Unknown _ = Unknown

resultOr :: PropResult -> PropResult -> PropResult
resultOr Satisfied _ = Satisfied
resultOr _ Satisfied = Satisfied
resultOr Unsatisfied x = x
resultOr Unknown _ = Unknown

resultNot :: PropResult -> PropResult
resultNot Satisfied = Unsatisfied
resultNot Unsatisfied = Unsatisfied
resultNot Unknown = Unknown

resultsAnd :: [PropResult] -> PropResult
resultsAnd = foldr resultAnd Satisfied

resultsOr :: [PropResult] -> PropResult
resultsOr = foldr resultOr Unsatisfied

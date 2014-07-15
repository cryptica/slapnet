{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Property
    (Property(..),
     showPropertyName,
     rename,
     PropertyType(..),
     Formula(..),
     LinearInequation(..),
     Op(..),
     Term(..))
where

data Term = Var String
          | Const Integer
          | Minus Term
          | Term :+: Term
          | Term :-: Term
          | Term :*: Term

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

data Op = Gt | Ge | Eq | Ne | Le | Lt

instance Show Op where
        show Gt = ">"
        show Ge = "≥"
        show Eq = "="
        show Ne = "≠"
        show Le = "≤"
        show Lt = "<"

-- TODO: merge LinIneq constructor into Formula
data LinearInequation = LinIneq Term Op Term

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

data PropertyType = Safety | Liveness

instance Show PropertyType where
        show Safety = "safety"
        show Liveness = "liveness"

data Property = Property {
        pname :: String,
        ptype :: PropertyType,
        pformula :: Formula
}

instance Show Property where
        show p =
            showPropertyName p ++ " { " ++ show (pformula p) ++ " }"

rename :: (String -> String) -> Property -> Property
rename f (Property pn pt pf) = Property pn pt (renameFormula f pf)

showPropertyName :: Property -> String
showPropertyName p = show (ptype p) ++ " property" ++
               (if null (pname p) then "" else " " ++ show (pname p))

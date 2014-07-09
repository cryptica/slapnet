{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Property
    (Property(..),
     showPropertyName,
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
        show (Minus t) = "-(" ++ show t ++ ")"
        show (t :+: u) = show t ++ " + " ++ show u
        show (t :-: u) = show t ++ " - " ++ show u
        show (t :*: u) = "(" ++ show t ++ ") * (" ++ show u ++ ")"

data Op = Gt | Ge | Eq | Ne | Le | Lt

instance Show Op where
        show Gt = ">"
        show Ge = "≥"
        show Eq = "="
        show Ne = "≠"
        show Le = "≤"
        show Lt = "<"

data LinearInequation = LinIneq Term Op Term

instance Show LinearInequation where
        show (LinIneq lhs op rhs) = show lhs ++ " " ++ show op ++ " " ++ show rhs

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

showPropertyName :: Property -> String
showPropertyName p = show (ptype p) ++ " property" ++
               (if null (pname p) then "" else " " ++ show (pname p))

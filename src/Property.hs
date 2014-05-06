{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Property
    (Property(..),
     PropertyType(..),
     Formula(..),
     LinearInequation(..),
     Op(..),
     Term(..),
     LinAtom(..))
where

import Data.List (intercalate)

data LinAtom = Var Integer String | Const Integer

instance Show LinAtom where
        show (Var c x) | c == 1 = x
        show (Var c x) | c == -1 = "-" ++ x
        show (Var c x) = show c ++ "*" ++ x
        show (Const c) = show c

data Term = Term [LinAtom]

instance Show Term where
        show (Term xs) = intercalate " + " (map show xs)

data Op = Gt | Ge | Eq | Le | Lt

instance Show Op where
        show Gt = ">"
        show Ge = "≥"
        show Eq = "="
        show Le = "≤"
        show Lt = "<"

data LinearInequation = LinIneq Term Op Term

instance Show LinearInequation where
        show (LinIneq lhs op rhs) = show lhs ++ " " ++ show op ++ " " ++ show rhs

data Formula = Atom LinearInequation
             | Neg Formula
             | Formula :&: Formula
             | Formula :|: Formula

infixr 3 :&:
infixr 2 :|:

instance Show Formula where
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
            show (ptype p) ++ " property " ++
            (if null (pname p) then "" else show (pname p) ++ " ") ++
            "{ " ++ show (pformula p) ++ " }"


{-# LANGUAGE OverloadedStrings #-}

module Printer.SARA
    (printProperties)
where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Monoid

import Printer
import PetriNet
import Property

renderSimpleTerm :: Integer -> Term -> Builder
renderSimpleTerm fac (Var x) = if fac == 1 then stringUtf8 x
                               else integerDec fac <> stringUtf8 x
renderSimpleTerm fac (Const c) = integerDec (fac*c)
renderSimpleTerm fac (Const c :*: t) = renderSimpleTerm (fac*c) t
renderSimpleTerm fac (t :*: Const c) = renderSimpleTerm (fac*c) t
renderSimpleTerm fac (Minus t) = renderSimpleTerm (-fac) t
renderSimpleTerm _ t = error $ "term not supported for sara: " <> show t

renderTerm :: Term -> Builder
renderTerm (t :+: u) = renderTerm t <> "+" <> renderSimpleTerm 1 u
renderTerm (t :-: u) = renderTerm t <> "+" <> renderSimpleTerm (-1) u
renderTerm t = renderSimpleTerm 1 t

renderOp :: Op -> Builder
renderOp Ge = ">"
renderOp Eq = ":"
renderOp Le = "<"
renderOp op = error $ "operand not supported for sara: " <> show op

renderLinIneq :: LinearInequation -> Builder
renderLinIneq (LinIneq lhs op (Const c)) =
        renderTerm lhs <> renderOp op <> integerDec c
renderLinIneq l = error $ "linear inequation not supported for sara: " <> show l

renderFormula :: Formula -> Builder
renderFormula (Atom a) = renderLinIneq a
renderFormula (Neg _) = error "negation not supported for sara"
renderFormula (p :&: q) = renderFormula p <> "," <> renderFormula q
renderFormula f = error $ "formula not supported for sara: " <> show f

renderProperty :: String -> PetriNet -> Property -> Builder
renderProperty filename net (Property propname Safety f) =
        "PROBLEM " <> stringUtf8 (validateId propname) <> ":\n" <>
        "GOAL REACHABILITY;\n" <>
        "FILE " <> stringUtf8 (reverse (takeWhile (/='/') (reverse filename)))
            <> " TYPE LOLA;\n" <>
        "INITIAL " <> intercalate ","
            (map (\(p,i) -> stringUtf8 p <> ":" <> integerDec i) (linitials net))
            <> ";\n" <>
        "FINAL COVER;\n" <>
        "CONSTRAINTS " <> renderFormula f <> ";"
renderProperty _ _ (Property _ Liveness _) =
        error "liveness property not supported for sara"

printProperties :: String -> PetriNet -> [Property] -> L.ByteString
printProperties filename net props =
        toLazyByteString $ intercalate "\n" $
            map (renderProperty filename net) props

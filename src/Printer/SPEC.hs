{-# LANGUAGE OverloadedStrings #-}

module Printer.SPEC
    (printProperty)
where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Monoid

import Property

renderOp :: Op -> Builder
renderOp Ge = ">="
renderOp op = error $ "operand not supported for spec: " <> show op

renderLinIneq :: LinearInequation -> Builder
renderLinIneq (LinIneq (Var x) op (Const c)) =
        stringUtf8 x <> renderOp op <> integerDec c
renderLinIneq l = error $ "linear inequation not supported for spe: " <> show l

renderConjunction :: Formula -> Builder
renderConjunction (Atom a) = renderLinIneq a
renderConjunction (Neg _) = error "negation not supported for spec"
renderConjunction (FTrue :&: p) = renderConjunction p
renderConjunction (p :&: FTrue) = renderConjunction p
renderConjunction (p :&: q) = renderConjunction p <> ", " <> renderConjunction q
renderConjunction f = error $ "formula not supported for spec: " <> show f

renderDisjunction :: Formula -> Builder
renderDisjunction (FFalse :|: p) = renderDisjunction p
renderDisjunction (p :|: FFalse) = renderDisjunction p
renderDisjunction (p :|: q) = renderDisjunction p <> "\n" <> renderDisjunction q
renderDisjunction f = renderConjunction f

renderFormula :: Formula -> Builder
renderFormula = renderDisjunction

renderProperty :: Property -> Builder
renderProperty (Property _ Safety f) = renderFormula f
renderProperty (Property _ Liveness _) =
        error "liveness property not supported for spec"

printProperty :: Property -> L.ByteString
printProperty prop =
        toLazyByteString $ renderProperty prop


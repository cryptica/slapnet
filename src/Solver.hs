{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Solver
    (prime,checkSat,ModelReader,val,vals,positiveVal,zeroVal,
     sumVal, getNames,makeVarMap,makeVarMapWith,mval,
     IntConstraint,BoolConstraint,IntResult,BoolResult,
     Model,ConstraintProblem,ConstraintProblem2,checkSat2,
     SIMap,SBMap,IMap,BMap)
where

import Data.SBV
import qualified Data.Map as M
import Control.Monad.Reader

import Util

type Model a = M.Map String a
type VarMap a = M.Map a String

type SIMap a = M.Map a SInteger
type SBMap a = M.Map a SBool
type IMap a = M.Map a Integer
type BMap a = M.Map a Bool

getNames :: VarMap a -> [String]
getNames = M.elems

type ModelReader a b = Reader (Model a) b
type IntConstraint = ModelReader SInteger SBool
type BoolConstraint = ModelReader SBool SBool
type IntResult a = ModelReader Integer a
type BoolResult a = ModelReader Bool a

type ConstraintProblem a b =
        (String, String, [String], ModelReader (SBV a) SBool, ModelReader a b)

-- TODO try this out
type ConstraintProblem2 a b =
        (String, String, [String], (String -> SBV a) -> SBool, (String -> a) -> b)

val :: (Ord a) => M.Map a b -> a -> b
val = (M.!)

mval :: (Ord a) => M.Map a b -> [a] -> [b]
mval = map . val

zeroVal :: (Ord a) => M.Map a SInteger -> a -> SBool
zeroVal m x = val m x .== 0

positiveVal :: (Ord a) => M.Map a SInteger -> a -> SBool
positiveVal m x = val m x .> 0

sumVal :: (Ord a, Num b) => M.Map a b -> b
sumVal = sum . vals

vals :: (Ord a) => M.Map a b -> [b]
vals = M.elems

makeVarMap :: (Show a, Ord a) => [a] -> VarMap a
makeVarMap = makeVarMapWith id

makeVarMapWith :: (Show a, Ord a) => (String -> String) -> [a] -> VarMap a
makeVarMapWith f xs = M.fromList $ xs `zip` map (f . show) xs

prime :: String -> String
prime = ('\'':)
{-
mVal :: Model a -> String -> a
mVal m x = M.findWithDefault (error ("key not found: " ++ x)) x m

mValues :: Model a -> [a]
mValues = M.elems

mElemsWith :: (a -> Bool) -> Model a -> [String]
mElemsWith f m = M.keys $ M.filter f m

mElemSum :: (Num a) => Model a -> [String] -> a
mElemSum m xs = sum $ map (mVal m) xs

class SModel a where
        mElem :: Model a -> String -> SBool
        mNotElem :: Model a -> String -> SBool
        mNotElem m x = bnot $ mElem m x
class CModel a where
        cElem :: Model a -> String -> Bool
        cNotElem :: Model a -> String -> Bool
        cNotElem m x = not $ cElem m x

instance SModel SInteger where
        mElem m x = mVal m x .> 0
        mNotElem m x = mVal m x .== 0
instance SModel SBool where
        mElem = mVal
        mNotElem m x = bnot $ mVal m x
instance CModel Integer where
        cElem m x = mVal m x > 0
        cNotElem m x = mVal m x == 0
instance CModel Bool where
        cElem = mVal
        cNotElem m x = not $ mVal m x
-}
symConstraints :: SymWord a => [String] -> ModelReader (SBV a) SBool ->
        Symbolic SBool
symConstraints vars constraint = do
        syms <- mapM exists vars
        return $ runReader constraint $ M.fromList $ vars `zip` syms

rebuildModel :: SymWord a => [String] -> Either String (Bool, [a]) ->
        Maybe (Model a)
rebuildModel _ (Left _) = Nothing
rebuildModel _ (Right (True, _)) = error "Prover returned unknown"
rebuildModel vars (Right (False, m)) = Just $ M.fromList $ vars `zip` m

checkSat :: (SatModel a, SymWord a, Show a, Show b) => Int ->
        ConstraintProblem a b -> IO (Maybe b)
checkSat verbosity (problemName, resultName, vars, constraint, interpretation) = do
        verbosePut verbosity 1 $ "Checking SAT of " ++ problemName
        result <- satWith z3{verbose=verbosity >= 4} $
            symConstraints vars constraint
        case rebuildModel vars (getModel result) of
            Nothing -> do
                verbosePut verbosity 2 "- unsat"
                return Nothing
            Just rawModel -> do
                verbosePut verbosity 2 "- sat"
                let model = runReader interpretation rawModel
                verbosePut verbosity 3 $ "- " ++ resultName ++ ": " ++ show model
                return $ Just model

symConstraints2 :: SymWord a => [String] -> ((String -> SBV a) -> SBool) ->
        Symbolic SBool
symConstraints2 vars constraint = do
        syms <- mapM exists vars
        let symMap = M.fromList $ vars `zip` syms
        let fm x = symMap M.! x
        return $ constraint fm

checkSat2 :: (SatModel a, SymWord a, Show a, Show b) => Int ->
        ConstraintProblem2 a b -> IO (Maybe b)
checkSat2 verbosity (problemName, resultName, vars, constraint, interpretation) = do
        verbosePut verbosity 1 $ "Checking SAT of " ++ problemName
        result <- satWith z3{verbose=verbosity >= 4} $
                    symConstraints2 vars constraint
        case rebuildModel vars (getModel result) of
            Nothing -> do
                verbosePut verbosity 2 "- unsat"
                return Nothing
            Just rawModel -> do
                verbosePut verbosity 2 "- sat"
                let fm x = rawModel M.! x
                let model = interpretation fm
                verbosePut verbosity 3 $ "- " ++ resultName ++ ": " ++ show model
                return $ Just model


{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Solver
    (prime,checkSat,ModelReader,val,vals,positiveVal,zeroVal,
     sumVal, getNames,makeVarMap,makeVarMapWith,mval,
     IntConstraint,BoolConstraint,IntResult,BoolResult,
     Model,ConstraintProblem,
     SIMap,SBMap,IMap,BMap,VarMap)
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

rebuildModel :: SymWord a => [String] -> Either String (Bool, [a]) ->
        Maybe (Model a)
rebuildModel _ (Left _) = Nothing
rebuildModel _ (Right (True, _)) = error "Prover returned unknown"
rebuildModel vars (Right (False, m)) = Just $ M.fromList $ vars `zip` m

symConstraints :: SymWord a => [String] -> ((String -> SBV a) -> SBool) ->
        Symbolic SBool
symConstraints vars constraint = do
        syms <- mapM exists vars
        let symMap = M.fromList $ vars `zip` syms
        let fm x = symMap M.! x
        return $ constraint fm

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
                let fm x = rawModel M.! x
                let model = interpretation fm
                verbosePut verbosity 3 $ "- " ++ resultName ++ ": " ++ show model
                return $ Just model


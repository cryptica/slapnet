{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Solver
    (prime,checkSat,val,vals,positiveVal,zeroVal,
     getNames,
     ConstraintProblem)
where

import Data.SBV
import qualified Data.Map as M

import Util

type ConstraintProblem a b =
        (String, String, [String], (String -> SBV a) -> SBool, (String -> a) -> b)

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
                verbosePut verbosity 4 $ "- raw model: " ++ show rawModel
                return $ Just model


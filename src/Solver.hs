{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Solver
    (prime,checkSat,checkSatMin,val,vals,positiveVal,zeroVal,
     getNames,
     ConstraintProblem)
where

import Data.SBV
import qualified Data.Map as M

import Util
import Options
import Control.Monad.IO.Class
import Control.Applicative

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
        return $ constraint $ val $ M.fromList $ vars `zip` syms

checkSat :: (SatModel a, SymWord a, Show a, Show b) =>
        ConstraintProblem a b -> OptIO (Maybe b)
checkSat (problemName, resultName, vars, constraint, interpretation) = do
        verbosePut 1 $ "Checking SAT of " ++ problemName
        verbosity <- opt optVerbosity
        result <- liftIO (satWith z3{verbose=verbosity >= 4}
                    (symConstraints vars constraint))
        case rebuildModel vars (getModel result) of
            Nothing -> do
                verbosePut 2 "- unsat"
                return Nothing
            Just rawModel -> do
                verbosePut 2 "- sat"
                let model = interpretation $ val rawModel
                verbosePut 3 $ "- " ++ resultName ++ ": " ++ show model
                verbosePut 4 $ "- raw model: " ++ show rawModel
                return $ Just model

checkSatMin :: (SatModel a, SymWord a, Show a, Show b) =>
        (Maybe Integer -> ConstraintProblem a (b, Integer)) -> OptIO (Maybe b)
checkSatMin minProblem = do
        optMin <- opt optMinimizeRefinement
        r0 <- checkSat $ minProblem Nothing
        case r0 of
            Nothing -> return Nothing
            Just (result, size) ->
                if optMin then
                    Just <$> findSmaller result size
                else
                    return $ Just result
    where findSmaller result size = do
            verbosePut 2 $ "Checking for size smaller than " ++ show size
            r1 <- checkSat $ minProblem (Just size)
            case r1 of
                Nothing -> return result
                Just (result', size') -> findSmaller result' size'


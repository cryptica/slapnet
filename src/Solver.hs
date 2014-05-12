module Solver
    (checkSat,ModelSI,ModelSB,ModelI,ModelB)
where

import Data.SBV
import qualified Data.Map as M

type ModelSI = M.Map String SInteger
type ModelSB = M.Map String SBool
type ModelI = M.Map String Integer
type ModelB = M.Map String Bool

symConstraints :: SymWord a => [String] -> (M.Map String (SBV a) -> SBool) ->
        Symbolic SBool
symConstraints vars constraint = do
        syms <- mapM exists vars
        return $ constraint $ M.fromList $ vars `zip` syms

rebuildModel :: SymWord a => [String] -> Either String (Bool, [a]) ->
        Maybe (M.Map String a)
rebuildModel _ (Left _) = Nothing
rebuildModel _ (Right (True, _)) = error "Prover returned unknown"
rebuildModel vars (Right (False, m)) = Just $ M.fromList $ vars `zip` m

checkSat :: (SatModel a, SymWord a) =>
        ([String], M.Map String (SBV a) -> SBool) ->
        IO (Maybe (M.Map String a))
checkSat (vars, constraint) = do
        result <- satWith z3{verbose=False} $ symConstraints vars constraint
        return $ rebuildModel vars $ getModel result

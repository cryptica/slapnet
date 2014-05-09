module Solver
    (checkSat,ModelSI,ModelSB,ModelI,ModelB)
where

import Data.SBV
import qualified Data.Map as M

type ModelSI = M.Map String SInteger
type ModelSB = M.Map String SBool
type ModelI = M.Map String Integer
type ModelB = M.Map String Bool
--type ModelLI = [(String, Integer)]
--type ModelLB = [(String, Bool)]

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
        result <- sat $ symConstraints vars constraint
        return $ rebuildModel vars $ getModel result

-- TODO: separate place and transition variables
--checkPropertyConstraintsSat :: PetriNet -> Property -> [[String]] -> IO (Maybe ModelLI)
--checkPropertyConstraintsSat net p traps =
--        let vars = places net ++ transitions net
--            cons m = checkPropertyPlusTrapConstraints (M.fromList m) net p traps
--        in  checkSat vars cons

--checkTrapConstraintsSat :: PetriNet -> ModelLI -> IO (Maybe ModelLB)
--checkTrapConstraintsSat net ma =
--        let vars = places net
--            cons m = checkAllTrapConstraints (M.fromList m) (M.fromList ma) net
--        in  checkSat vars cons


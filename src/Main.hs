module Main where

import System.Environment (getArgs)

import Parser (parseFile)
import PetriNet
import Property
import Solver
import Solver.StateEquation
import Solver.TransitionInvariant
import Solver.TrapConstraints

-- TODO: check type of property and only do trap refinement for safety
-- properties

checkSafetyProperty :: PetriNet -> Formula -> [[String]] -> IO Bool
checkSafetyProperty net f traps = do
    r <- checkSat $ checkStateEquationSat net f traps
    case r of
        Nothing -> return True
        Just a -> do
            --print a
            let assigned = markedPlacesFromAssignment net a
            putStrLn $ "Assignment found marking " ++ show assigned
            rt <- checkSat $ checkTrapSat net assigned
            case rt of
                Nothing -> do
                    putStrLn "No trap found."
                    return False
                Just at -> do
                    let trap = trapFromAssignment at
                    putStrLn $ "Trap found with places " ++ show trap
                    checkSafetyProperty net f (trap:traps)

checkLivenessProperty :: PetriNet -> Formula -> IO Bool
checkLivenessProperty net f = do
        r <- checkSat $ checkTransitionInvariantSat net f
        case r of
            Nothing -> return True
            Just m -> do
                putStrLn "Assignment found:"
                print m
                return False


checkProperty :: PetriNet -> Property -> IO Bool
checkProperty net p = do
        r <- case ptype p of
            Safety -> checkSafetyProperty net (pformula p) []
            Liveness -> checkLivenessProperty net (pformula p)
        putStrLn $ if r then "Property is satisfied."
                        else "Property may not be satisfied."
        return r

--checkPropertyWithTrapRefinement :: PetriNet -> Property -> [[String]] -> IO ()
--checkPropertyWithTrapRefinement net p traps = do
--        r <- checkPropertyConstraintsSat net p traps
--        case r of
--            Nothing -> putStrLn "Property is satisfied"
--            Just m -> do
--                putStrLn "Property may not satisfied, model:" >> print m
--                r2 <- checkTrapConstraintsSat net m
--                case r2 of
--                    Nothing -> putStrLn "No trap found"
--                    Just m2 -> do
--                        let trap = map fst $ filter snd m2
--                        putStrLn "Trap found:" >> print trap
--                        checkPropertyWithTrapRefinement net p (trap:traps)

main :: IO ()
main = do
        args <- getArgs
        let file = head args
        putStrLn "SLAPnet - Safety and Liveness Analysis of Petri Nets with SMT solvers\n"
        putStrLn $ "Reading \"" ++ file ++ "\""
        (net,properties) <- parseFile file
        putStrLn $ "Analyzing " ++ showName net
        mapM_ (\p -> do
                  putStrLn $ "\nChecking " ++ show p
                  checkProperty net p
              ) properties


module Main where

import System.Environment (getArgs)
import System.Exit

import Parser (parseFile)
import PetriNet
import Property
import Solver
import Solver.StateEquation
import Solver.TrapConstraints
import Solver.TransitionInvariant
import Solver.SComponent

checkSafetyProperty :: PetriNet -> Formula -> [[String]] -> IO Bool
checkSafetyProperty net f traps = do
        r <- checkSat $ checkStateEquationSat net f traps
        case r of
            Nothing -> return True
            Just a -> do
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
            Just a -> do
                let fired = firedTransitionsFromAssignment a
                putStrLn $ "Assignment found firing " ++ show fired
                rt <- checkSat $ checkSComponentSat net a
                case rt of
                    Nothing -> do
                        putStrLn "No S-component found"
                        return False
                    Just at -> do
                        --let trap = trapFromAssignment at
                        putStrLn $ "S-component found: " ++ show at
                        -- checkLivenessProperty net f (trap:traps)
                        return False

checkProperty :: PetriNet -> Property -> IO Bool
checkProperty net p = do
        --putStrLn $ "\nChecking " ++ showPropertyName p
        r <- case ptype p of
            Safety -> checkSafetyProperty net (pformula p) []
            Liveness -> checkLivenessProperty net (pformula p)
        putStrLn $ if r then "Property is satisfied."
                        else "Property may not be satisfied."
        return r

main :: IO ()
main = do
        args <- getArgs
        let file = head args
        putStrLn "SLAPnet - Safety and Liveness Analysis of Petri Nets with SMT solvers\n"
        putStrLn $ "Reading \"" ++ file ++ "\""
        (net,properties) <- parseFile file
        putStrLn $ "Analyzing " ++ showNetName net
        rs <- mapM (checkProperty net) properties
        if and rs then
            exitSuccess
        else
            exitWith $ ExitFailure 2


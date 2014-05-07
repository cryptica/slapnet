module Main where

import System.Environment (getArgs)

import Parser (parseFile)
import PetriNet
import Property
import Solver

checkProperty :: PetriNet -> Property -> IO ()
checkProperty net p = do
        r <- checkPropertyConstraintsSat net p []
        case r of
            Nothing -> putStrLn "Property satisfied"
            Just m -> putStrLn "Property may not satisfied, model:" >> print m

checkPropertyWithTrapRefinement :: PetriNet -> Property -> [[String]] -> IO ()
checkPropertyWithTrapRefinement net p traps = do
        r <- checkPropertyConstraintsSat net p traps
        case r of
            Nothing -> putStrLn "Property satisfied"
            Just m -> do
                putStrLn "Property not satisfied, model:" >> print m
                r2 <- checkTrapConstraintsSat net m
                case r2 of
                    Nothing -> putStrLn "No trap found"
                    Just m2 -> do
                        let trap = map fst $ filter snd m2
                        putStrLn "Trap found:" >> print trap
                        checkPropertyWithTrapRefinement net p (trap:traps)

main :: IO ()
main = do
        args <- getArgs
        let file = head args
        putStrLn "Safety and Liveness Analysis of Petri Nets with SMT solvers"
        putStrLn $ "Reading \"" ++ file ++ "\""
        (net,properties) <- parseFile file
        putStrLn $ "Analyzing " ++ showName net
        mapM_ (\p -> do
                  putStrLn $ "Checking " ++ show p
                  checkPropertyWithTrapRefinement net p []
              ) properties


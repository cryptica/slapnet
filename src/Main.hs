module Main where

import System.Environment (getArgs)

import Parser (parseFile)
import PetriNet
import Property
import Solver

checkProperty :: PetriNet -> Property -> IO ()
checkProperty net p = do
        r <- checkPropertyConstraintsSat net p
        case r of
            Nothing -> putStrLn "Property satisfied"
            Just m -> putStrLn "Property not satisfied, model:" >> print m

--checkPropertyWithTrapRefinement :: PetriNet -> Property -> IO ()

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
                  checkProperty net p
              ) properties


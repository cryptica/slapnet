module Main where

import System.Environment (getArgs)

import Parser (parseFile)
import PetriNet
import Property
import Solver

checkProperty :: PetriNet -> Property -> IO String
checkProperty net p = do
        r <- checkSat net p
        return (if r then "Property not satisfied"
                     else "Property satisfied")

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
                  r <- checkProperty net p
                  putStrLn r
              ) properties


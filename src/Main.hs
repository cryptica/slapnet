module Main where

import System.Environment (getArgs)

import Parser (parseFile)
import PetriNet
import Property

checkProperty :: PetriNet -> Property -> Bool
checkProperty net p = True

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
                  putStrLn $ show $ checkProperty net p
              ) properties


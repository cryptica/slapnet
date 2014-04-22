module Main where

import System.Environment (getArgs)

import Parser (parseFile)

main :: IO ()
main = do
        args <- getArgs
        putStrLn "Safety and Liveness Analysis of Petri Nets with SMT solvers"
        let file = head args
        net <- parseFile file
        print net


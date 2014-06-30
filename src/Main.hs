{-# LANGUAGE TupleSections #-}

module Main where

import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Console.GetOpt
import Control.Monad
import Control.Applicative
import Data.Char (toUpper)

import Parser (parseFile)
import PetriNet
import Property
import Solver
import Solver.StateEquation
import Solver.TrapConstraints
import Solver.TransitionInvariant
import Solver.SComponent

data InputFormat = PNET | LOLA | TPN deriving (Show,Read)

data Options = Options { inputFormat :: InputFormat
                       , optVerbose :: Bool
                       , optShowHelp :: Bool
                       , optShowVersion :: Bool
                       }

startOptions :: Options
startOptions = Options { inputFormat = PNET
                       , optVerbose = False
                       , optShowHelp = False
                       , optShowVersion = False
                       }

options :: [ OptDescr (Options -> Either String Options) ]
options =
        [ Option "f" ["format"]
        (ReqArg (\arg opt ->
            case reads (map toUpper arg) of
                [(format, "")] -> Right opt { inputFormat = format }
                _ -> Left ("invalid input format `" ++ arg ++ "'\n"))
        "FORMAT")
        ("Input format (possible values=\"pnet\", \"lola\", \"tpn\"\n" ++
         "              default=\"pnet\")")

        , Option "v" ["verbose"]
        (NoArg (\opt -> Right opt { optVerbose = True }))
        "Enable verbose messages"

        , Option "V" ["version"]
        (NoArg (\opt -> Right opt { optShowVersion = True }))
        "Show version"

        , Option "h" ["help"]
        (NoArg (\opt -> Right opt { optShowHelp = True }))
        "Show help"
        ]

parseArgs :: IO (Either String (Options, [String]))
parseArgs = do
        args <- getArgs
        case getOpt Permute options args of
            (actions, files, []) ->
                return $ (,files) <$> foldl (>>=) (return startOptions) actions
            (_, _, errs) -> return $ Left $ concat errs

checkFile :: Options -> String -> IO Bool
checkFile opts file = do
        putStrLn $ "Reading \"" ++ file ++ "\""
        (net,properties) <- parseFile file
        putStrLn $ "Analyzing " ++ showNetName net
        when (optVerbose opts) (do
                putStrLn $ "Places: " ++ show (length  $ places net)
                putStrLn $ "Transitions: " ++ show (length $ transitions net)
            )
        rs <- mapM (checkProperty (optVerbose opts) net) properties
        putStrLn ""
        return $ and rs

checkProperty :: Bool -> PetriNet -> Property -> IO Bool
checkProperty verbose net p = do
        putStrLn $ "\nChecking " ++ showPropertyName p
        r <- case ptype p of
            Safety -> checkSafetyProperty verbose net (pformula p) []
            Liveness -> checkLivenessProperty verbose net (pformula p) []
        putStrLn $ if r then "Property is satisfied."
                        else "Property may not be satisfied."
        return r

checkSafetyProperty :: Bool -> PetriNet -> Formula -> [[String]] -> IO Bool
checkSafetyProperty verbose net f traps = do
        r <- checkSat $ checkStateEquationSat net f traps
        case r of
            Nothing -> return True
            Just a -> do
                let assigned = markedPlacesFromAssignment net a
                putStrLn "Assignment found"
                when verbose (putStrLn $ "Places marked: " ++ show assigned)
                rt <- checkSat $ checkTrapSat net assigned
                case rt of
                    Nothing -> do
                        putStrLn "No trap found."
                        return False
                    Just at -> do
                        let trap = trapFromAssignment at
                        putStrLn "Trap found"
                        when verbose (putStrLn $ "Places in trap: " ++ show trap)
                        checkSafetyProperty verbose net f (trap:traps)

checkLivenessProperty :: Bool -> PetriNet -> Formula -> [([String],[String])] -> IO Bool
checkLivenessProperty verbose net f strans = do
        r <- checkSat $ checkTransitionInvariantSat net f strans
        case r of
            Nothing -> return True
            Just ax -> do
                let fired = firedTransitionsFromAssignment ax
                putStrLn "Assignment found"
                when verbose (putStrLn $ "Transitions fired: " ++ show fired)
                rt <- checkSat $ checkSComponentSat net fired ax
                case rt of
                    Nothing -> do
                        putStrLn "No S-component found"
                        return False
                    Just as -> do
                        let sOutIn = getSComponentOutIn net ax as
                        putStrLn "S-component found"
                        when verbose (putStrLn $ "Out/In: " ++ show sOutIn)
                        checkLivenessProperty verbose net f (sOutIn:strans)

main :: IO ()
main = do
        putStrLn "SLAPnet - Safety and Liveness Analysis of Petri Nets with SMT solvers\n"
        args <- parseArgs
        case args of
            Left err -> exitErrorWith err
            Right (opts, files) -> do

                when (optShowVersion opts) (exitSuccessWith "Version 0.01")
                when (optShowHelp opts) (exitSuccessWith
                     (usageInfo "SLAPnet" options))
                when (null files) (exitErrorWith "No input file given")
                rs <- mapM (checkFile opts) files
                if and rs then
                    exitSuccessWith "All properties satisfied."
                else
                    exitFailureWith "Some properties may not be satisfied."

exitSuccessWith :: String -> IO ()
exitSuccessWith msg = do
        putStrLn msg
        exitSuccess

exitFailureWith :: String -> IO ()
exitFailureWith msg = do
        putStrLn msg
        exitWith $ ExitFailure 1

exitErrorWith :: String -> IO ()
exitErrorWith msg = do
        hPutStrLn stderr msg
        exitWith $ ExitFailure 2

{-# LANGUAGE TupleSections #-}

module Main where

import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Console.GetOpt
import Control.Monad
import Control.Applicative

import Parser
import qualified Parser.PNET as PNET
import qualified Parser.TPN as TPN
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
                       , proveTermination :: Bool
                       }

startOptions :: Options
startOptions = Options { inputFormat = PNET
                       , optVerbose = False
                       , optShowHelp = False
                       , optShowVersion = False
                       , proveTermination = False
                       }

options :: [ OptDescr (Options -> Either String Options) ]
options =
        [ Option "" ["pnet"]
        (NoArg (\opt -> Right opt { inputFormat = PNET }))
        "Use the pnet input format"

        , Option "" ["lola"]
        (NoArg (\opt -> Right opt { inputFormat = LOLA }))
        "Use the lola input format"

        , Option "" ["tpn"]
        (NoArg (\opt -> Right opt { inputFormat = TPN }))
        "Use the tpn input format"

        , Option "" ["termination"]
        (NoArg (\opt -> Right opt { proveTermination = True }))
        "Prove termination"

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

checkFile :: Parser (PetriNet,[Property]) -> Bool -> [Property] ->
            String -> IO Bool
checkFile parser verbose addedProperties file = do
        putStrLn $ "Reading \"" ++ file ++ "\""
        (net,properties) <- parseFile parser file
        putStrLn $ "Analyzing " ++ showNetName net
        when verbose (do
                putStrLn $ "Places: " ++ show (length  $ places net)
                putStrLn $ "Transitions: " ++ show (length $ transitions net)
            )
        rs <- mapM (checkProperty verbose net) (addedProperties ++ properties)
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
                let parser = case inputFormat opts of
                                 PNET -> PNET.parseContent
                                 LOLA -> error "lola is not supported yet"
                                 TPN -> TPN.parseContent
                let properties = [ Property "termination" Liveness FTrue
                                 | proveTermination opts ]
                rs <- mapM (checkFile parser (optVerbose opts) properties) files
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

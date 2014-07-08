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
import qualified Parser.LOLA as LOLA
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
                       , optVerbosity :: Int
                       , optShowHelp :: Bool
                       , optShowVersion :: Bool
                       , proveTermination :: Bool
                       , optRefine :: Bool
                       }

startOptions :: Options
startOptions = Options { inputFormat = PNET
                       , optVerbosity = 1
                       , optShowHelp = False
                       , optShowVersion = False
                       , proveTermination = False
                       , optRefine = True
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

        , Option "n" ["no-refinement"]
        (NoArg (\opt -> Right opt { optRefine = False }))
        "Don't use refinement"

        , Option "v" ["verbose"]
        (NoArg (\opt -> Right opt { optVerbosity = optVerbosity opt + 1 }))
        "Increase verbosity (may be specified more than once)"

        , Option "q" ["quiet"]
        (NoArg (\opt -> Right opt { optVerbosity = optVerbosity opt - 1 }))
        "Decrease verbosity (may be specified more than once)"

        , Option "V" ["version"]
        (NoArg (\opt -> Right opt { optShowVersion = True }))
        "Show version"

        , Option "h" ["help"]
        (NoArg (\opt -> Right opt { optShowHelp = True }))
        "Show help"
        ]

verbosePut :: Int -> Int -> String -> IO ()
verbosePut verbosity level str =
        when (verbosity >= level) (putStrLn str)

parseArgs :: IO (Either String (Options, [String]))
parseArgs = do
        args <- getArgs
        case getOpt Permute options args of
            (actions, files, []) ->
                return $ (,files) <$> foldl (>>=) (return startOptions) actions
            (_, _, errs) -> return $ Left $ concat errs

checkFile :: Parser (PetriNet,[Property]) -> Int -> Bool -> [Property] ->
            String -> IO Bool
checkFile parser verbosity refine addedProperties file = do
        verbosePut verbosity 0 $ "Reading \"" ++ file ++ "\""
        (net,properties) <- parseFile parser file
        verbosePut verbosity 1 $ "Analyzing " ++ showNetName net
        verbosePut verbosity 2 $
                "Places: " ++ show (length  $ places net) ++ "\n" ++
                "Transitions: " ++ show (length $ transitions net)
        rs <- mapM (checkProperty verbosity net refine)
                  (addedProperties ++ properties)
        verbosePut verbosity 0 ""
        return $ and rs

checkProperty :: Int -> PetriNet -> Bool -> Property -> IO Bool
checkProperty verbosity net refine p = do
        verbosePut verbosity 1 $ "\nChecking " ++ showPropertyName p
        r <- case ptype p of
            Safety -> checkSafetyProperty verbosity net refine (pformula p) []
            Liveness -> checkLivenessProperty verbosity net refine (pformula p) []
        verbosePut verbosity 0 $ showPropertyName p ++
                                    if r then "is satisfied."
                                    else " may not be satisfied."
        return r

checkSafetyProperty :: Int -> PetriNet -> Bool ->
        Formula -> [[String]] -> IO Bool
checkSafetyProperty verbosity net refine f traps = do
        r <- checkSat $ checkStateEquationSat net f traps
        case r of
            Nothing -> return True
            Just a -> do
                let assigned = markedPlacesFromAssignment net a
                verbosePut verbosity 1 "Assignment found"
                verbosePut verbosity 2 $ "Places marked: " ++ show assigned
                if refine then do
                    rt <- checkSat $ checkTrapSat net assigned
                    case rt of
                        Nothing -> do
                            putStrLn "No trap found."
                            return False
                        Just at -> do
                            let trap = trapFromAssignment at
                            verbosePut verbosity 1 "Trap found"
                            verbosePut verbosity 2 $ "Places in trap: " ++
                                                      show trap
                            checkSafetyProperty verbosity net refine f
                                                (trap:traps)
                else
                    return False

checkLivenessProperty :: Int -> PetriNet -> Bool ->
        Formula -> [([String],[String])] -> IO Bool
checkLivenessProperty verbosity net refine f strans = do
        r <- checkSat $ checkTransitionInvariantSat net f strans
        case r of
            Nothing -> return True
            Just ax -> do
                let fired = firedTransitionsFromAssignment ax
                verbosePut verbosity 1 "Assignment found"
                verbosePut verbosity 2 $ "Transitions fired: " ++ show fired
                if refine then do
                    rt <- checkSat $ checkSComponentSat net fired ax
                    case rt of
                        Nothing -> do
                            verbosePut verbosity 1 "No S-component found"
                            return False
                        Just as -> do
                            let sOutIn = getSComponentOutIn net ax as
                            verbosePut verbosity 1 "S-component found"
                            verbosePut verbosity 2 $ "Out/In: " ++ show sOutIn
                            checkLivenessProperty verbosity net refine f
                                                  (sOutIn:strans)
                else
                    return False

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
                let verbosity = optVerbosity opts
                    refinement = optRefine opts
                let parser = case inputFormat opts of
                                 PNET -> PNET.parseContent
                                 LOLA -> LOLA.parseContent
                                 TPN -> TPN.parseContent
                let properties = [ Property "termination" Liveness FTrue
                                 | proveTermination opts ]
                rs <- mapM (checkFile parser verbosity refinement properties)
                          files
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

{-# LANGUAGE TupleSections #-}

module Main where

import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Console.GetOpt
import Control.Monad
import Control.Applicative ((<$>))

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

-- TODO: Change NoDeadlockOutOf to NoDeadlockUnless=FILE
data ImplicitProperty = Termination
                      | NoDeadlock | NoDeadlockOutOf String
                      | Safe | Bounded Integer
                      deriving (Show,Read)

data Options = Options { inputFormat :: InputFormat
                       , optVerbosity :: Int
                       , optShowHelp :: Bool
                       , optShowVersion :: Bool
                       , optProperties :: [ImplicitProperty]
                       , optRefine :: Bool
                       }

startOptions :: Options
startOptions = Options { inputFormat = PNET
                       , optVerbosity = 1
                       , optShowHelp = False
                       , optShowVersion = False
                       , optProperties = []
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
        (NoArg (\opt -> Right opt {
                           optProperties = Termination : optProperties opt
               }))
        "Prove termination"

        , Option "" ["no-deadlock"]
        (NoArg (\opt -> Right opt {
                           optProperties = NoDeadlock : optProperties opt
               }))
        "Prove that there is no deadlock"

        , Option "" ["no-deadlock-out-of"]
        (ReqArg (\arg opt -> Right opt {
                           optProperties = NoDeadlockOutOf arg : optProperties opt
                })
                "PLACE")
        "Prove that there is no deadlock unless PLACE is marked"

        , Option "" ["safe"]
        (NoArg (\opt -> Right opt {
                           optProperties = Safe : optProperties opt
               }))
        "Prove that the net is safe, i.e. 1-bounded"

        , Option "" ["bounded"]
        (ReqArg (\arg opt -> case reads arg of
                    [(k, "")] -> Right opt {
                           optProperties = Bounded k : optProperties opt }
                    _ -> Left ("invalid argument for k-bounded option: " ++ arg)
                )
                "K")
        "Prove that the net is K-bounded"

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

checkFile :: Parser (PetriNet,[Property]) -> Int -> Bool ->
            [ImplicitProperty] -> String -> IO Bool
checkFile parser verbosity refine implicitProperties file = do
        verbosePut verbosity 0 $ "Reading \"" ++ file ++ "\""
        (net,properties) <- parseFile parser file
        verbosePut verbosity 1 $ "Analyzing " ++ showNetName net
        verbosePut verbosity 2 $
                "Places: " ++ show (length  $ places net) ++ "\n" ++
                "Transitions: " ++ show (length $ transitions net)
        let addedProperties = map (makeImplicitProperty net) implicitProperties
        print properties
        rs <- mapM (checkProperty verbosity net refine)
                  (addedProperties ++ properties)
        verbosePut verbosity 0 ""
        return $ and rs

placeOp :: Op -> (String, Integer) -> Formula
placeOp op (p,w) = Atom $ LinIneq (Var p) op (Const w)

makeImplicitProperty :: PetriNet -> ImplicitProperty -> Property
makeImplicitProperty _ Termination = Property "termination" Liveness FTrue
makeImplicitProperty net NoDeadlock =
        Property "no deadlock" Safety $
            foldl (:&:) FTrue
                (map (foldl (:|:) FFalse . map (placeOp Lt) . lpre net)
                     (transitions net))
makeImplicitProperty net (NoDeadlockOutOf pl) =
        Property ("no deadlock out of " ++ pl) Safety $
            placeOp Lt (pl,1) :&: pformula (makeImplicitProperty net NoDeadlock)
makeImplicitProperty net (Bounded k) =
        Property (show k ++ "-bounded") Safety $
            foldl (:|:) FFalse
                (map (\p -> placeOp Gt (p,k)) (places net))
makeImplicitProperty net Safe =
        Property "safe" Safety $ pformula (makeImplicitProperty net (Bounded 1))

checkProperty :: Int -> PetriNet -> Bool -> Property -> IO Bool
checkProperty verbosity net refine p = do
        verbosePut verbosity 1 $ "\nChecking " ++ showPropertyName p
        r <- case ptype p of
            Safety -> checkSafetyProperty verbosity net refine (pformula p) []
            Liveness -> checkLivenessProperty verbosity net refine (pformula p) []
        verbosePut verbosity 0 $ showPropertyName p ++
                                    if r then " is satisfied."
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
                let properties = optProperties opts
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
        exitWith $ ExitFailure 2

exitErrorWith :: String -> IO ()
exitErrorWith msg = do
        hPutStrLn stderr msg
        exitWith $ ExitFailure 3

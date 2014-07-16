{-# LANGUAGE TupleSections #-}

module Main where

import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Console.GetOpt
import Control.Monad
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.List (partition)

import Parser
import qualified Parser.PNET as PNET
import qualified Parser.LOLA as LOLA
import qualified Parser.TPN as TPN
import PetriNet
import Printer
import qualified Printer.LOLA as LOLAPrinter
import qualified Printer.SARA as SARAPrinter
import Property
import Solver
import Solver.StateEquation
import Solver.TrapConstraints
import Solver.TransitionInvariant
import Solver.SComponent

data InputFormat = PNET | LOLA | TPN deriving (Show,Read)

data NetTransformation = TerminationByReachability
                       | ValidateIdentifiers

data ImplicitProperty = Termination
                      | NoDeadlock | NoDeadlockUnlessFinal
                      | ProperTermination
                      | Safe | Bounded Integer
                      deriving (Show,Read)

data Options = Options { inputFormat :: InputFormat
                       , optVerbosity :: Int
                       , optShowHelp :: Bool
                       , optShowVersion :: Bool
                       , optProperties :: [ImplicitProperty]
                       , optTransformations :: [NetTransformation]
                       , optRefine :: Bool
                       , optOutput :: Maybe String
                       }

startOptions :: Options
startOptions = Options { inputFormat = PNET
                       , optVerbosity = 1
                       , optShowHelp = False
                       , optShowVersion = False
                       , optProperties = []
                       , optTransformations = []
                       , optRefine = True
                       , optOutput = Nothing
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

        , Option "" ["validate-identifiers"]
        (NoArg (\opt -> Right opt {
            optTransformations = ValidateIdentifiers : optTransformations opt
          }))
        "Make identifiers valid for lola"

        , Option "" ["termination-by-reachability"]
        (NoArg (\opt -> Right opt {
            optTransformations = TerminationByReachability : optTransformations opt
          }))
        "Prove termination by reducing it to reachability"

        , Option "" ["terminating"]
        (NoArg (\opt -> Right opt {
                   optProperties = Termination : optProperties opt
               }))
        "Prove that the net is terminating"

        , Option "" ["proper-termination"]
        (NoArg (\opt -> Right opt {
                   optProperties = ProperTermination : optProperties opt
               }))
        "Prove termination in the final marking"

        , Option "" ["deadlock-free"]
        (NoArg (\opt -> Right opt {
                   optProperties = NoDeadlock : optProperties opt
               }))
        "Prove that the net is deadlock-free"

        , Option "" ["deadlock-free-unless-final"]
        (NoArg (\opt -> Right opt {
                   optProperties = NoDeadlockUnlessFinal : optProperties opt
               }))
        ("Prove that the net is deadlock-free\n" ++
         "unless it is in the final marking")

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

        , Option "o" ["output"]
        (ReqArg (\arg opt -> Right opt {
                        optOutput = Just arg
                })
                "FILE")
        "Write net and properties to FILE"

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

writeFiles :: Int -> String -> PetriNet -> [Property] -> IO ()
writeFiles verbosity basename net props = do
        verbosePut verbosity 1 $ "Writing " ++ showNetName net ++ " to " ++ basename
        writeFile basename $ LOLAPrinter.printNet net
        mapM_ (\(p,i) -> do
                let file = basename ++ ".task" ++ show i
                verbosePut verbosity 1 $ "Writing " ++ showPropertyName p ++
                                         " to " ++ file
                writeFile file $ LOLAPrinter.printProperty p
              ) (zip props [(1::Integer)..])
        verbosePut verbosity 1 $ "Writing properties to " ++ basename ++ ".sara"
        writeFile (basename ++ ".sara") $ unlines $
                map (SARAPrinter.printProperty basename net) props

checkFile :: Parser (PetriNet,[Property]) -> Int -> Bool ->
            [ImplicitProperty] -> [NetTransformation] ->
            Maybe String -> String -> IO Bool
checkFile parser verbosity refine implicitProperties transformations
          output file = do
        verbosePut verbosity 0 $ "Reading \"" ++ file ++ "\""
        (net,props) <- parseFile parser file
        let props' = props ++ map (makeImplicitProperty net) implicitProperties
        let (net',props'') = foldl transformNet (net,props') transformations
        verbosePut verbosity 1 $ "Analyzing " ++ showNetName net
        verbosePut verbosity 2 $
                "Places: " ++ show (length  $ places net') ++ "; " ++
                "Transitions: " ++ show (length $ transitions net')
        verbosePut verbosity 3 $ show net'
        rs <- mapM (checkProperty verbosity net' refine) props''
        case output of
            Just outputfile -> writeFiles verbosity outputfile net' props''
            Nothing -> return ()
        verbosePut verbosity 0 ""
        return $ and rs

placeOp :: Op -> (String, Integer) -> Formula
placeOp op (p,w) = Atom $ LinIneq (Var p) op (Const w)

transformNet :: (PetriNet, [Property]) -> NetTransformation ->
               (PetriNet, [Property])
transformNet (net, props) TerminationByReachability =
        let prime = ('\'':)
            ps = ["'sigma", "'m1", "'m2"] ++
                 places net ++ map prime (places net)
            is = [("'m1", 1)] ++
                 initials net ++ map (first prime) (initials net)
            ts = ("'switch", [("'m1",1)], [("'m2",1)]) :
                 concatMap (\t ->
                    let (preT, postT) = context net t
                        pre'  = [("'m1",1)] ++ preT  ++ map (first prime) preT
                        post' = [("'m1",1)] ++ postT ++ map (first prime) postT
                        pre''  = ("'m2",1) : map (first prime) preT
                        post'' = [("'m2",1), ("'sigma",1)] ++ map (first prime) postT
                    in  [(t, pre', post'), (prime t, pre'', post'')]
                 )
                 (transitions net)
            prop = Property "termination by reachability" Safety $
                    foldl (:&:) (Atom (LinIneq (Var "'sigma") Ge (Const 1)))
                      (map (\p -> Atom (LinIneq (Var (prime p)) Ge (Var p)))
                        (places net))
            -- TODO: map existing liveness properties
        in  (makePetriNetWithTrans (name net) ps ts is, prop : props)
transformNet (net, props) ValidateIdentifiers =
        let ps = map validateId $ places net
            ts = map validateId $ transitions net
            is = map (first validateId) $ initials net
            as = map (\(a,b,x) -> (validateId a, validateId b, x)) $ arcs net
            net' = makePetriNet (name net) ps ts as is
            props' = map (rename validateId) props
        in  (net', props')

makeImplicitProperty :: PetriNet -> ImplicitProperty -> Property
makeImplicitProperty _ Termination =
        Property "termination" Liveness FTrue
makeImplicitProperty net ProperTermination =
        let (finals, nonfinals) = partition (null . lpost net) (places net)
        in  Property "proper termination" Safety
            (foldl (:|:) FFalse (map (\p -> placeOp Gt (p,0)) finals) :&:
             foldl (:|:) FFalse (map (\p -> placeOp Gt (p,0)) nonfinals))
makeImplicitProperty net NoDeadlock =
        Property "no deadlock" Safety $
            foldl (:&:) FTrue
                (map (foldl (:|:) FFalse . map (placeOp Lt) . lpre net)
                     (transitions net))
makeImplicitProperty net NoDeadlockUnlessFinal =
        let nodeadlock = makeImplicitProperty net NoDeadlock
            (finals, nonfinals) = partition (null . lpost net) (places net)
        in  Property "no deadlock unless final" Safety $
            (foldl (:&:) FTrue  (map (\p -> placeOp Eq (p,0)) finals) :|:
             foldl (:|:) FFalse (map (\p -> placeOp Gt (p,0)) nonfinals)) :&:
            pformula nodeadlock
makeImplicitProperty net (Bounded k) =
        Property (show k ++ "-bounded") Safety $
            foldl (:|:) FFalse
                (map (\p -> placeOp Gt (p,k)) (places net))
makeImplicitProperty net Safe =
        let bounded = makeImplicitProperty net (Bounded 1)
        in  Property "safe" Safety $ pformula bounded

checkProperty :: Int -> PetriNet -> Bool -> Property -> IO Bool
checkProperty verbosity net refine p = do
        verbosePut verbosity 1 $ "\nChecking " ++ showPropertyName p
        verbosePut verbosity 3 $ show p
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
                verbosePut verbosity 3 $ "Assignment: " ++ show a
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
                            verbosePut verbosity 3 $ "Trap assignment: " ++
                                                      show at
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
                verbosePut verbosity 3 $ "Assignment: " ++ show ax
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
                            verbosePut verbosity 3 $ "S-Component assignment: " ++
                                                      show as
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
                let properties = reverse $ optProperties opts
                let transformations = reverse $ optTransformations opts
                rs <- mapM (checkFile parser verbosity refinement properties
                                transformations (optOutput opts)) files
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

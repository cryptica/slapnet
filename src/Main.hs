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
import qualified Data.ByteString.Lazy as L

import Util
import Parser
import qualified Parser.PNET as PNET
import qualified Parser.LOLA as LOLA
import qualified Parser.TPN as TPN
import qualified Parser.MIST as MIST
import PetriNet
import Printer
import qualified Printer.LOLA as LOLAPrinter
import qualified Printer.SARA as SARAPrinter
import qualified Printer.SPEC as SPECPrinter
import qualified Printer.DOT as DOTPrinter
import Property
import Structure
import Solver
import Solver.StateEquation
import Solver.TrapConstraints
import Solver.TransitionInvariant
--import Solver.LivenessInvariant
--import Solver.SComponent
--import Solver.CommFreeReachability

data InputFormat = PNET | LOLA | TPN | MIST deriving (Show,Read)
data OutputFormat = OutLOLA | OutSARA | OutSPEC | OutDOT deriving (Read)

instance Show OutputFormat where
        show OutLOLA = "LOLA"
        show OutSARA = "SARA"
        show OutSPEC = "SPEC"
        show OutDOT = "DOT"

data NetTransformation = TerminationByReachability
                       | ValidateIdentifiers

data ImplicitProperty = Termination
                      | DeadlockFree | DeadlockFreeUnlessFinal
                      | FinalStateUnreachable
                      | ProperTermination
                      | Safe | Bounded Integer
                      | StructFreeChoice
                      | StructParallel
                      | StructFinalPlace
                      | StructCommunicationFree
                      deriving (Show,Read)

data Options = Options { inputFormat :: InputFormat
                       , optVerbosity :: Int
                       , optShowHelp :: Bool
                       , optShowVersion :: Bool
                       , optProperties :: [ImplicitProperty]
                       , optTransformations :: [NetTransformation]
                       , optRefine :: Bool
                       , optInvariant :: Bool
                       , optOutput :: Maybe String
                       , outputFormat :: OutputFormat
                       , optUseProperties :: Bool
                       , optPrintStructure :: Bool
                       }

startOptions :: Options
startOptions = Options { inputFormat = PNET
                       , optVerbosity = 1
                       , optShowHelp = False
                       , optShowVersion = False
                       , optProperties = []
                       , optTransformations = []
                       , optRefine = True
                       , optInvariant = False
                       , optOutput = Nothing
                       , outputFormat = OutLOLA
                       , optUseProperties = True
                       , optPrintStructure = False
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

        , Option "" ["spec"]
        (NoArg (\opt -> Right opt { inputFormat = MIST }))
        "Use the mist input format"

        , Option "s" ["structure"]
        (NoArg (\opt -> Right opt { optPrintStructure = True }))
        "Print structural information"

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

        , Option "" ["termination"]
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
                   optProperties = DeadlockFree : optProperties opt
               }))
        "Prove that the net is deadlock-free"

        , Option "" ["deadlock-free-unless-final"]
        (NoArg (\opt -> Right opt {
                   optProperties = DeadlockFreeUnlessFinal : optProperties opt
               }))
        ("Prove that the net is deadlock-free\n" ++
         "unless it is in the final marking")

        , Option "" ["final-state-unreachable"]
        (NoArg (\opt -> Right opt {
                   optProperties = FinalStateUnreachable : optProperties opt
               }))
        "Prove that the final state is unreachable"

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

        , Option "" ["free-choice"]
        (NoArg (\opt -> Right opt {
                   optProperties = StructFreeChoice : optProperties opt
               }))
        "Prove that the net is free-choice"

        , Option "" ["parallel"]
        (NoArg (\opt -> Right opt {
                   optProperties = StructParallel : optProperties opt
               }))
        "Prove that the net has non-trivial parallellism"

        , Option "" ["final-place"]
        (NoArg (\opt -> Right opt {
                   optProperties = StructFinalPlace : optProperties opt
               }))
        "Prove that there is only one needed final place"

        , Option "" ["communication-free"]
        (NoArg (\opt -> Right opt {
                   optProperties = StructCommunicationFree : optProperties opt
               }))
        "Prove that the net is communication-free"

        , Option "n" ["no-refinement"]
        (NoArg (\opt -> Right opt { optRefine = False }))
        "Don't use refinement"

        , Option "i" ["invariant"]
        (NoArg (\opt -> Right opt { optInvariant = True }))
        "Try to generate an invariant"

        , Option "o" ["output"]
        (ReqArg (\arg opt -> Right opt {
                        optOutput = Just arg
                })
                "FILE")
        "Write net and properties to FILE"

        , Option "" ["out-lola"]
        (NoArg (\opt -> Right opt { outputFormat = OutLOLA }))
        "Use the lola output format"

        , Option "" ["out-sara"]
        (NoArg (\opt -> Right opt { outputFormat = OutSARA }))
        "Use the sara output format"

        , Option "" ["out-spec"]
        (NoArg (\opt -> Right opt { outputFormat = OutSPEC }))
        "Use the spec output format"

        , Option "" ["out-dot"]
        (NoArg (\opt -> Right opt { outputFormat = OutDOT }))
        "Use the dot output format"

        , Option "" ["no-given-properties"]
        (NoArg (\opt -> Right opt {
                   optUseProperties = False
               }))
        "Do not use the properties given in the input file"

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

parseArgs :: IO (Either String (Options, [String]))
parseArgs = do
        args <- getArgs
        case getOpt Permute options args of
            (actions, files, []) ->
                return $ (,files) <$> foldl (>>=) (return startOptions) actions
            (_, _, errs) -> return $ Left $ concat errs

writeFiles :: Int -> String -> OutputFormat -> PetriNet -> [Property] -> IO ()
writeFiles verbosity basename format net props = do
        verbosePut verbosity 1 $ "Writing " ++ showNetName net ++ " to " ++
                                  basename ++ " in format " ++ show format
        case format of
            OutLOLA -> do
                L.writeFile basename $ LOLAPrinter.printNet net
                mapM_ (\(p,i) -> do
                        let file = basename ++ ".task" ++ show i
                        verbosePut verbosity 1 $ "Writing " ++ showPropertyName p
                                                 ++ " to " ++ file
                        L.writeFile file $ LOLAPrinter.printProperty p
                      ) (zip props [(1::Integer)..])
            OutSARA -> do
                L.writeFile basename $ LOLAPrinter.printNet net
                verbosePut verbosity 1 $ "Writing properties to " ++ basename ++
                                         ".sara"
                L.writeFile (basename ++ ".sara") $
                    SARAPrinter.printProperties basename net props
            OutSPEC ->
                mapM_ (\(p,i) -> do
                        let file = basename ++ ".target" ++ show i
                        verbosePut verbosity 1 $ "Writing " ++ showPropertyName p
                                                 ++ " to " ++ file
                        L.writeFile file $ SPECPrinter.printProperty p
                      ) (zip props [(1::Integer)..])
            OutDOT ->
                L.writeFile basename $ DOTPrinter.printNet net

structuralAnalysis :: PetriNet -> IO ()
structuralAnalysis net =  do
        let noGhost t = t `notElem` ghostTransitions net
        let initP  = filter (\x -> (not . any noGhost . pre net) x &&
                             (any noGhost . post net) x) (places net)
        let finalP = filter (\x -> (not . any noGhost . post net) x &&
                             (any noGhost . pre net) x) (places net)
        let isolP  = filter (\x -> (not . any noGhost . post net) x &&
                             (not . any noGhost . pre net) x) (places net)
        let initT  = filter (\t -> noGhost t && null (pre  net t))
                        (transitions net)
        let finalT = filter (\t -> noGhost t && null (post net t))
                        (transitions net)
        putStrLn $ "Places             : " ++ show (length (places net))
        putStrLn $ "Transitions        : " ++ show (length (transitions net))
        putStrLn $ "Initial places     : " ++ show (length initP)
        putStrLn $ "Initial transitions: " ++ show (length initT)
        putStrLn $ "Isolated places    : " ++ show (length isolP)
        putStrLn $ "Final places       : " ++ show (length finalP)
        putStrLn $ "Final transitions  : " ++ show (length finalT)

checkFile :: Parser (PetriNet,[Property]) -> Int -> Bool -> Bool ->
            [ImplicitProperty] -> [NetTransformation] ->
            Bool -> Maybe String -> OutputFormat -> Bool -> String ->
            IO PropResult
checkFile parser verbosity refine invariant implicitProperties transformations
          useProperties output format printStruct file = do
        verbosePut verbosity 0 $ "Reading \"" ++ file ++ "\""
        (net,props) <- parseFile parser file
        let props' = if useProperties then props else []
        let props'' = props' ++ map (makeImplicitProperty net) implicitProperties
        let (net',props''') = foldl transformNet (net,props'') transformations
        verbosePut verbosity 1 $ "Analyzing " ++ showNetName net
        verbosePut verbosity 2 $
                "Places: " ++ show (length  $ places net') ++ "; " ++
                "Transitions: " ++ show (length $ transitions net')
        when printStruct $ structuralAnalysis net
        verbosePut verbosity 3 $ show net'
        case output of
            Just outputfile ->
                writeFiles verbosity outputfile format net' props'''
            Nothing -> return ()
        -- TODO: short-circuit?
        rs <- mapM (checkProperty verbosity net' refine invariant) props'''
        verbosePut verbosity 0 ""
        return $ resultsAnd rs

placeOp :: Op -> (Place, Integer) -> Formula Place
placeOp op (p,w) = LinearInequation (Var p) op (Const w)

transformNet :: (PetriNet, [Property]) -> NetTransformation ->
               (PetriNet, [Property])
transformNet (net, props) TerminationByReachability =
        let m1 = Place "'m1"
            m2 = Place "'m1"
            sigma = Place "'sigma"
            switch = Transition "'switch"
            primePlace = renamePlace prime
            primeTransition = renameTransition prime
            ps = [sigma, m1, m2] ++
                 places net ++ map primePlace (places net)
            is = [(Place "'m1", 1)] ++
                 linitials net ++ map (first primePlace) (linitials net)
            transformTransition t =
                let (preT, postT) = context net t
                    pre'  = [(m1,1)] ++ preT  ++ map (first primePlace) preT
                    post' = [(m1,1)] ++ postT ++ map (first primePlace) postT
                    pre''  = (m2,1) : map (first primePlace) preT
                    post'' = [(m2,1), (sigma,1)] ++ map (first primePlace) postT
                in  if t `elem` ghostTransitions net then
                        [(t, (pre', post'))]
                    else
                        [(t, (pre', post')), (primeTransition t, (pre'', post''))]
            ts = (switch, ([(m1,1)], [(m2,1)])) :
                 concatMap transformTransition (transitions net)
            gs = ghostTransitions net
            prop = Property "termination by reachability" $ Safety $
                    foldl (:&:) (LinearInequation (Var sigma) Ge (Const 1))
                      (map (\p -> LinearInequation
                                (Var (primePlace p) :-: Var p) Ge (Const 0))
                        (places net))
            -- TODO: map existing liveness properties
        in  (makePetriNetWith (name net) ps ts is gs, prop : props)
transformNet (net, props) ValidateIdentifiers =
        (renamePetriNetPlacesAndTransitions validateId net,
         map (renameProperty validateId) props)

makeImplicitProperty :: PetriNet -> ImplicitProperty -> Property
makeImplicitProperty net Termination =
        Property "termination" $ Liveness $
            foldl (:&:) FTrue
                (map (\t -> LinearInequation (Var t) Eq (Const 0))
                    (ghostTransitions net))
makeImplicitProperty net ProperTermination =
        let (finals, nonfinals) = partition (null . lpost net) (places net)
        in  Property "proper termination" $ Safety
            (foldl (:|:) FFalse (map (\p -> placeOp Gt (p,0)) finals) :&:
             foldl (:|:) FFalse (map (\p -> placeOp Gt (p,0)) nonfinals))
makeImplicitProperty net DeadlockFree =
        Property "deadlock-free" $ Safety $
            foldl (:&:) FTrue
                (map (foldl (:|:) FFalse . map (placeOp Lt) . lpre net)
                     (filter (`notElem` ghostTransitions net) (transitions net)))
makeImplicitProperty net DeadlockFreeUnlessFinal =
        let (Property _ (Safety pf)) = makeImplicitProperty net DeadlockFree
            (finals, nonfinals) = partition (null . lpost net) (places net)
        in  Property "deadlock-free unless final" $ Safety $
            (foldl (:&:) FTrue  (map (\p -> placeOp Eq (p,0)) finals) :|:
             foldl (:|:) FFalse (map (\p -> placeOp Gt (p,0)) nonfinals)) :&:
            pf
makeImplicitProperty net FinalStateUnreachable =
        let (finals, nonfinals) = partition (null . lpost net) (places net)
        in  Property "final state unreachable" $ Safety $
             foldl (:|:) FFalse (map (\p -> placeOp Gt (p,0)) finals) :&:
             foldl (:&:) FTrue (map (\p -> placeOp Eq (p,0)) nonfinals)
makeImplicitProperty net (Bounded k) =
        Property (show k ++ "-bounded") $ Safety $
            foldl (:|:) FFalse
                (map (\p -> placeOp Ge (p,k+1))
                    (filter (`notElem` concatMap (post net) (ghostTransitions net))
                        (places net)))
makeImplicitProperty net Safe =
        let bounded = makeImplicitProperty net (Bounded 1)
        in  Property "safe" $ pcont bounded
makeImplicitProperty _ StructFreeChoice =
        Property "free choice" $ Structural FreeChoice
makeImplicitProperty _ StructParallel =
        Property "parallel" $ Structural Parallel
makeImplicitProperty _ StructFinalPlace =
        Property "final place" $ Structural FinalPlace
makeImplicitProperty _ StructCommunicationFree =
        Property "communication free" $ Structural CommunicationFree

checkProperty :: Int -> PetriNet -> Bool -> Bool -> Property -> IO PropResult
checkProperty verbosity net refine invariant p = do
        verbosePut verbosity 1 $ "\nChecking " ++ showPropertyName p
        verbosePut verbosity 3 $ show p
        r <- case pcont p of
            (Safety pf) -> checkSafetyProperty verbosity net refine invariant pf
            (Liveness pf) -> checkLivenessProperty verbosity net refine invariant pf
            --(Structural ps) -> checkStructuralProperty verbosity net ps
        verbosePut verbosity 0 $ showPropertyName p ++ " " ++
            case r of
                Satisfied -> "is satisfied."
                Unsatisfied -> "is not satisfied."
                Unknown-> "may not be satisfied."
        return r

checkSafetyProperty :: Int -> PetriNet -> Bool -> Bool ->
        Formula Place -> IO PropResult
checkSafetyProperty verbosity net refine invariant f =
        -- TODO: add flag for this kind of structural check
        --if checkCommunicationFree net then do
        --    verbosePut verbosity 1 "Net found to be communication-free"
        --    checkSafetyPropertyByCommFree verbosity net f
        --else
        do
            r <- checkSafetyPropertyBySafetyRef verbosity net refine f []
            if r == Satisfied && invariant then
                -- TODO: add invariant generation
                error "Invariant generation for safety properties not yet supported"
            else
                return r
{-
checkSafetyPropertyByCommFree :: Int -> PetriNet -> Formula -> IO PropResult
checkSafetyPropertyByCommFree verbosity net f = do
        r <- checkSat $ checkCommFreeReachabilitySat net f
        case r of
            Nothing -> return Satisfied
            Just a -> do
                verbosePut verbosity 1 "Assignment found"
                verbosePut verbosity 3 $ "Assignment: " ++ show a
                return Unsatisfied
-}
checkSafetyPropertyBySafetyRef :: Int -> PetriNet -> Bool ->
        Formula Place -> [Trap] -> IO PropResult
checkSafetyPropertyBySafetyRef verbosity net refine f traps = do
        r <- checkSat verbosity $ checkStateEquationSat net f traps
        case r of
            Nothing -> return Satisfied
            Just marking -> do
                if refine then do
                    rt <- checkSat verbosity $ checkTrapSat net marking
                    case rt of
                        Nothing -> do
                            verbosePut verbosity 1 "No trap found."
                            return Unknown
                        Just trap -> do
                            checkSafetyPropertyBySafetyRef verbosity net
                                                refine f (trap:traps)
                else
                    return Unknown

checkLivenessProperty :: Int -> PetriNet -> Bool -> Bool ->
        Formula Transition -> IO PropResult
checkLivenessProperty verbosity net refine invariant f = do
        (r, comps) <- checkLivenessPropertyByRef verbosity net refine f []
        return r
        --if r == Satisfied && invariant then
        --    generateLivenessInvariant verbosity net f comps
        --else
        --    return r
{-
generateLivenessInvariant :: Int -> PetriNet ->
        Formula -> [SCompCut] -> IO PropResult
generateLivenessInvariant verbosity net f comps = do
        verbosePut verbosity 1 "Generating invariant"
        let cuts = generateCuts f comps
        r <- checkSat $ checkLivenessInvariantSat net cuts
        case r of
            Nothing -> return Unsatisfied
            Just as -> do
                let inv = getLivenessInvariant net cuts as
                mapM_ print inv
                return Satisfied
-}
checkLivenessPropertyByRef :: Int -> PetriNet -> Bool ->
        Formula Transition -> [Cut] -> IO (PropResult, [Cut])
checkLivenessPropertyByRef verbosity net refine f cuts = do
        r <- checkSat verbosity $ checkTransitionInvariantSat net f cuts
        case r of
            Nothing -> return (Satisfied, cuts)
            Just x -> do
                if refine then do
                    rt <- return Nothing -- checkSat $ checkSComponentSat net x
                    case rt of
                        Nothing -> do
                            return (Unknown, cuts)
                        Just cut -> do
                            checkLivenessPropertyByRef verbosity net refine f
                                                  (cut:cuts)
                else
                    return (Unknown, cuts)

checkStructuralProperty :: Int -> PetriNet -> Structure -> IO PropResult
checkStructuralProperty _ net struct =
        if checkStructure net struct then
            return Satisfied
        else
            return Unsatisfied

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
                    invariant = optInvariant opts
                let parser = case inputFormat opts of
                                 PNET -> PNET.parseContent
                                 LOLA -> LOLA.parseContent
                                 TPN -> TPN.parseContent
                                 MIST -> MIST.parseContent
                let properties = reverse $ optProperties opts
                let transformations = reverse $ optTransformations opts
                rs <- mapM (checkFile parser verbosity refinement invariant
                     properties transformations (optUseProperties opts)
                     (optOutput opts) (outputFormat opts) (optPrintStructure opts))
                     files
                -- TODO: short-circuit with Control.Monad.Loops?
                case resultsAnd rs of
                    Satisfied ->
                        exitSuccessWith "All properties satisfied."
                    Unsatisfied ->
                        exitFailureWith "Some properties are not satisfied"
                    Unknown ->
                        exitFailureWith "Some properties may not be satisfied."

-- TODO: Always exit with exit code 0 unless an error occured
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

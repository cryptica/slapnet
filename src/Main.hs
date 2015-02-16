module Main where

import System.Exit
import System.IO
import Control.Monad
import Control.Concurrent.ParallelIO
import Control.Arrow (first)
import Data.List (partition)
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Control.Monad.Reader

import Util
import Options
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
import Solver.SubnetEmptyTrap
import Solver.LivenessInvariant
import Solver.SafetyInvariant
import Solver.SComponentWithCut
import Solver.SComponent
import Solver.Simplifier
--import Solver.Interpolant
--import Solver.CommFreeReachability

writeFiles :: String -> PetriNet -> [Property] -> OptIO ()
writeFiles basename net props = do
        format <- opt outputFormat
        verbosePut 1 $ "Writing " ++ showNetName net ++ " to " ++
                                  basename ++ " in format " ++ show format
        case format of
            OutLOLA -> do
                liftIO $ L.writeFile basename $ LOLAPrinter.printNet net
                mapM_ (\(p,i) -> do
                        let file = basename ++ ".task" ++ show i
                        verbosePut 1 $ "Writing " ++ showPropertyName p
                                                 ++ " to " ++ file
                        liftIO $ L.writeFile file $ LOLAPrinter.printProperty p
                      ) (zip props [(1::Integer)..])
            OutSARA -> do
                liftIO $ L.writeFile basename $ LOLAPrinter.printNet net
                verbosePut 1 $ "Writing properties to " ++ basename ++
                                         ".sara"
                liftIO $ L.writeFile (basename ++ ".sara") $
                    SARAPrinter.printProperties basename net props
            OutSPEC ->
                mapM_ (\(p,i) -> do
                        let file = basename ++ ".target" ++ show i
                        verbosePut 1 $ "Writing " ++ showPropertyName p
                                                 ++ " to " ++ file
                        liftIO $ L.writeFile file $ SPECPrinter.printProperty p
                      ) (zip props [(1::Integer)..])
            OutDOT ->
                liftIO $ L.writeFile basename $ DOTPrinter.printNet net

structuralAnalysis :: PetriNet -> OptIO ()
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
        verbosePut 0 $ "Places             : " ++ show (length (places net))
        verbosePut 0 $ "Transitions        : " ++ show (length (transitions net))
        verbosePut 0 $ "Initial places     : " ++ show (length initP)
        verbosePut 0 $ "Initial transitions: " ++ show (length initT)
        verbosePut 0 $ "Isolated places    : " ++ show (length isolP)
        verbosePut 0 $ "Final places       : " ++ show (length finalP)
        verbosePut 0 $ "Final transitions  : " ++ show (length finalT)

checkFile :: String -> OptIO PropResult
checkFile file = do
        verbosePut 0 $ "Reading \"" ++ file ++ "\""
        format <- opt inputFormat
        let parser = case format of
                             PNET -> PNET.parseContent
                             LOLA -> LOLA.parseContent
                             TPN -> TPN.parseContent
                             MIST -> MIST.parseContent
        (net,props) <- liftIO $ parseFile parser file
        useProperties <- opt optUseProperties
        let props' = if useProperties then props else []
        implicitProperties <- opt optProperties
        let props'' = props' ++ map (makeImplicitProperty net) implicitProperties
        transformations <- opt optTransformations
        let (net',props''') = foldl transformNet (net,props'') transformations
        verbosePut 1 $ "Analyzing " ++ showNetName net
        verbosePut 2 $
                "Places: " ++ show (length  $ places net') ++ "; " ++
                "Transitions: " ++ show (length $ transitions net')
        printStruct <- opt optPrintStructure
        when printStruct $ structuralAnalysis net
        verbosePut 3 $ show net'
        output <- opt optOutput
        case output of
            Just outputfile ->
                writeFiles outputfile net' props'''
            Nothing -> return ()
        -- TODO: short-circuit?
        rs <- mapM (checkProperty net') props'''
        verbosePut 0 ""
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
        in  (makePetriNetWithTrans (name net) ps ts is gs, prop : props)
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

checkProperty :: PetriNet -> Property -> OptIO PropResult
checkProperty net p = do
        verbosePut 1 $ "\nChecking " ++ showPropertyName p
        verbosePut 3 $ show p
        r <- case pcont p of
            (Safety pf) -> checkSafetyProperty net pf
            (Liveness pf) -> checkLivenessProperty net pf
            (Structural ps) -> checkStructuralProperty net ps
        verbosePut 0 $ showPropertyName p ++ " " ++
            case r of
                Satisfied -> "is satisfied."
                Unsatisfied -> "is not satisfied."
                Unknown-> "may not be satisfied."
        return r

checkSafetyProperty :: PetriNet ->
        Formula Place -> OptIO PropResult
checkSafetyProperty net f = do
        r <- checkSafetyProperty' net f []
        case r of
            (Nothing, traps) -> do
                invariant <- opt optInvariant
                if invariant then
                    checkSat (checkSafetyInvariantSat net f traps) >>= printInvariant
                else
                    return Satisfied
            (Just _, _) ->
                return Unknown

printInvariant :: (Show a, Invariant a) => Maybe [a] -> OptIO PropResult
printInvariant invResult =
        case invResult of
            Nothing -> do
                verbosePut 0 "No invariant found"
                return Unknown
            Just inv -> do
                verbosePut 0 "Invariant found"
                verbosePut 2 $ "Number of atoms in invariants: " ++ show (map invariantSize inv)
                mapM_ (putLine . show) inv
                return Satisfied

checkSafetyProperty' :: PetriNet ->
        Formula Place -> [Trap] -> OptIO (Maybe Marking, [Trap])
checkSafetyProperty' net f traps = do
        r <- checkSat $ checkStateEquationSat net f traps
        case r of
            Nothing -> return (Nothing, traps)
            Just m -> do
                refine <- opt optRefinementType
                if isJust refine then
                    refineSafetyProperty net f traps m
                else
                    return (Just m, traps)

refineSafetyProperty :: PetriNet ->
        Formula Place -> [Trap] -> Marking -> OptIO (Maybe Marking, [Trap])
refineSafetyProperty net f traps m = do
        r <- checkSat $ checkTrapSat net m
        case r of
            Nothing ->
                return (Just m, traps)
            Just trap ->
                checkSafetyProperty' net f (trap:traps)

checkLivenessProperty :: PetriNet ->
        Formula Transition -> OptIO PropResult
checkLivenessProperty net f = do
        (r, cuts) <- checkLivenessProperty' net f []
        verbosePut 2 $ "Number of refinements: " ++ show (length cuts)
        case r of
            Nothing -> do
                invariant <- opt optInvariant
                if invariant then
                    getLivenessInvariant net f cuts >>= printInvariant
                else
                    return Satisfied
            Just _ ->
                return Unknown

getLivenessInvariant :: PetriNet -> Formula Transition -> [Cut] -> OptIO (Maybe [LivenessInvariant])
getLivenessInvariant net f cuts = do
        dnfCuts <- generateCuts net f cuts
        verbosePut 2 $ "Number of disjuncts: " ++ show (length dnfCuts)
        invs <- parallelIO (map (checkSat . checkLivenessInvariantSat net f) dnfCuts)
        let cutInvs = map (Just . cutToLivenessInvariant) cuts
        return $ sequence (invs ++ cutInvs)

checkLivenessProperty' :: PetriNet ->
        Formula Transition -> [Cut] -> OptIO (Maybe FiringVector, [Cut])
checkLivenessProperty' net f cuts = do
        r <- checkSat $ checkTransitionInvariantSat net f cuts
        case r of
            Nothing -> return (Nothing, cuts)
            Just x -> do
                rt <- findLivenessRefinement net x
                case rt of
                    Nothing ->
                        return (Just x, cuts)
                    Just cut ->
                        checkLivenessProperty' net f (cut:cuts)

findLivenessRefinement :: PetriNet -> FiringVector ->
        OptIO (Maybe Cut)
findLivenessRefinement net x = do
        refinementType <- opt optRefinementType
        case refinementType of
            Just TrapRefinement ->
                findLivenessRefinementByEmptyTraps net (initialMarking net) x []
            Just SComponentRefinement -> do
                r1 <- findLivenessRefinementBySComponent net x
                case r1 of
                    Nothing -> findLivenessRefinementByEmptyTraps net
                                                      (initialMarking net) x []
                    Just _ -> return r1
            Just SComponentWithCutRefinement -> do
                r1 <- findLivenessRefinementBySComponentWithCut net x
                case r1 of
                    Nothing -> findLivenessRefinementByEmptyTraps net
                                                      (initialMarking net) x []
                    Just _ -> return r1
            Nothing -> return Nothing

findLivenessRefinementBySComponent :: PetriNet -> FiringVector ->
        OptIO (Maybe Cut)
findLivenessRefinementBySComponent net x =
        checkSatMin $ checkSComponentSat net x

findLivenessRefinementBySComponentWithCut :: PetriNet -> FiringVector ->
        OptIO (Maybe Cut)
findLivenessRefinementBySComponentWithCut net x =
        checkSatMin $ checkSComponentWithCutSat net x

findLivenessRefinementByEmptyTraps :: PetriNet -> Marking -> FiringVector ->
        [Trap] -> OptIO (Maybe Cut)
findLivenessRefinementByEmptyTraps net m x traps = do
        r <- checkSatMin $ checkSubnetEmptyTrapSat net m x
        case r of
            Nothing -> do
                rm <- refineSafetyProperty net FTrue traps m
                case rm of
                    (Nothing, _) -> do
                        cut <- generateLivenessRefinement net x traps
                        return $ Just cut
                    (Just _, _) ->
                        return Nothing
            Just trap -> do
                let traps' = trap:traps
                rm <- local (\opts -> opts { optRefinementType = Nothing }) $
                            checkSafetyProperty' net FTrue traps'
                case rm of
                    (Nothing, _) -> do
                        cut <- generateLivenessRefinement net x traps'
                        return $ Just cut
                    (Just m', _) ->
                        findLivenessRefinementByEmptyTraps net m' x traps'

generateLivenessRefinement :: PetriNet -> FiringVector -> [Trap] -> OptIO Cut
generateLivenessRefinement net x traps = do
        -- TODO: also use better cuts for traps
        let cut = constructCut net x traps
        verbosePut 3 $ "- cut: " ++ show cut
        return cut

checkStructuralProperty :: PetriNet -> Structure -> OptIO PropResult
checkStructuralProperty net struct =
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
                when (optShowHelp opts) (exitSuccessWith usageInformation)
                when (null files) (exitErrorWith "No input file given")
                let opts' = opts {
                        optProperties = reverse (optProperties opts),
                        optTransformations= reverse (optTransformations opts)
                    }
                rs <- runReaderT (mapM checkFile files) opts'
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
        cleanupAndExitWith ExitSuccess

exitFailureWith :: String -> IO ()
exitFailureWith msg = do
        putStrLn msg
        cleanupAndExitWith $ ExitFailure 2

exitErrorWith :: String -> IO ()
exitErrorWith msg = do
        hPutStrLn stderr msg
        cleanupAndExitWith $ ExitFailure 3

cleanupAndExitWith :: ExitCode -> IO ()
cleanupAndExitWith code = do
        stopGlobalPool
        exitWith code

module Solver.SComponent
    (checkSComponentSat)
where

import Data.SBV
import Data.List (partition)
import Control.Monad
import qualified Data.Map as M

import Util
import PetriNet
import Solver

checkPrePostPlaces :: PetriNet -> VarMap Place -> VarMap Transition ->
        IntConstraint
checkPrePostPlaces net p' t' =
            liftM bAnd $ mapM checkPrePostPlace $ places net
        where checkPrePostPlace p = do
                incoming <- mapM (positiveVal t') $ pre net p
                outgoing <- mapM (positiveVal t') $ post net p
                pVal <- positiveVal p' p
                return $ pVal ==> bAnd incoming &&& bAnd outgoing

checkPrePostTransitions :: PetriNet -> VarMap Place -> VarMap Transition ->
        IntConstraint
checkPrePostTransitions net p' t' =
            liftM bAnd $ mapM checkPrePostTransition $ transitions net
        where checkPrePostTransition t = do
                incoming <- mval p' $ pre net t
                outgoing <- mval p' $ post net t
                tVal <- positiveVal t' t
                return $ tVal ==> sum incoming .== 1 &&& sum outgoing .== 1

checkSubsetTransitions :: FiringVector ->
        VarMap Transition -> VarMap Transition -> IntConstraint
checkSubsetTransitions x t' y = do
            yset <- mapM checkTransition $ elems x
            ys <- sumVal y
            ts <- liftM sum $ mval t' $ elems x
            return $ bAnd yset &&& ys .< ts
        where checkTransition t = do
                yt <- positiveVal y t
                tt <- positiveVal t' t
                return $ yt ==> tt

checkNotEmpty :: VarMap Transition -> IntConstraint
checkNotEmpty y = liftM (.>0) $ sumVal y

checkClosed :: PetriNet -> FiringVector -> VarMap Place ->
        VarMap Transition -> IntConstraint
checkClosed net x p' y =
            liftM bAnd $ mapM checkPlaceClosed $ places net
        where checkPlaceClosed p = do
                  pVal <- positiveVal p' p
                  postVal <- liftM bAnd $
                                mapM checkTransition
                                    [(t,t') | t <- pre net p, t' <- post net p,
                                              value x t > 0, value x t' > 0 ]
                  return $ pVal ==> postVal
              checkTransition (t,t') = do
                  tPre <- positiveVal y t
                  tPost <- positiveVal y t'
                  return $ tPre ==> tPost

checkTokens :: PetriNet -> VarMap Place -> IntConstraint
checkTokens net p' =
            liftM ((.==1) . sum) $ mapM addPlace $ linitials net
        where addPlace (p,i) = do
                  v <- val p' p
                  return $ literal i * v

checkBinary :: VarMap Place -> VarMap Transition ->
        VarMap Transition -> IntConstraint
checkBinary p' t' y = do
            pc <- checkBins p'
            tc <- checkBins t'
            yc <- checkBins y
            return $ pc &&& tc &&& yc
        where checkBins xs = do
                  vs <- vals xs
                  return $ bAnd $ map (\x -> x .== 0 ||| x .== 1) vs


checkSComponent :: PetriNet -> FiringVector -> VarMap Place ->
        VarMap Transition -> VarMap Transition -> IntConstraint
checkSComponent net x p' t' y = do
        c1 <- checkPrePostPlaces net p' t'
        c2 <- checkPrePostTransitions net p' t'
        c3 <- checkSubsetTransitions x t' y
        c4 <- checkNotEmpty y
        c5 <- checkClosed net x p' y
        c6 <- checkTokens net p'
        c7 <- checkBinary p' t' y
        return $ c1 &&& c2 &&& c3 &&& c4 &&& c5 &&& c6 &&& c7

checkSComponentSat :: PetriNet -> FiringVector ->
        ConstraintProblem Integer Cut
checkSComponentSat net x =
        let fired = elems x
            p' = makeVarMap $ places net
            t' = makeVarMap $ transitions net
            y = makeVarMapWith prime fired
        in  ("S-component constraints", "cut",
            getNames p' ++ getNames t' ++ getNames y,
            checkSComponent net x p' t' y,
            cutFromAssignment x t' y)

-- TODO: use strongly connected components and min cuts
cutFromAssignment :: FiringVector ->
        VarMap Transition -> VarMap Transition -> IntResult Cut
cutFromAssignment x t' y = do
        tm <- valMap t'
        ym <- valMap y
        let (ts, u) = partition (\t -> value x t > 0) $ M.keys $ M.filter (> 0) tm
        let (t1, t2) = partition (\t -> ym M.! t > 0) ts
        return ([t1,t2], u)


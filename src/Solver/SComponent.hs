module Solver.SComponent
    (checkSComponent,checkSComponentSat,
     getSComponentOutIn)
where

import Data.SBV
import Data.List (partition)

import PetriNet
import Solver

prime :: String -> String
prime = ('\'':)

checkPrePostPlaces :: PetriNet -> ModelSI -> SBool
checkPrePostPlaces net m =
            bAnd $ map checkPrePostPlace $ places net
        where checkPrePostPlace p =
                let incoming = map (mElem m) $ pre net p
                    outgoing = map (mElem m) $ post net p
                in  mElem m p ==> bAnd incoming &&& bAnd outgoing

checkPrePostTransitions :: PetriNet -> ModelSI -> SBool
checkPrePostTransitions net m =
            bAnd $ map checkPrePostTransition $ transitions net
        where checkPrePostTransition t =
                let incoming = mElemSum m $ pre net t
                    outgoing = mElemSum m $ post net t
                in  mElem m t ==> incoming .== 1 &&& outgoing .== 1

checkSubsetTransitions :: [String] -> ModelSI -> SBool
checkSubsetTransitions fired m =
            bAnd (map checkTransition fired) &&&
            mElemSum m (map prime fired) .< mElemSum m fired
        where checkTransition t =
                mElem m (prime t) ==> mElem m t

checkNotEmpty :: [String] -> ModelSI -> SBool
checkNotEmpty fired m = mElemSum m (map prime fired) .> 0

checkClosed :: PetriNet -> ModelI -> ModelSI -> SBool
checkClosed net ax m =
            bAnd (map checkPlaceClosed (places net))
        where checkPlaceClosed p = mElem m p ==>
                        bAnd (map checkTransition
                             [(t,t') | t <- pre net p, t' <- post net p,
                                       cElem ax t, cElem ax t' ])
              checkTransition (t,t') =
                  mElem m (prime t) ==> mElem m (prime t')

checkTokens :: PetriNet -> ModelSI -> SBool
checkTokens net m =
            sum (map addPlace (initials net)) .== 1
        where addPlace (p,x) = literal x * mVal m p

checkBinary :: ModelSI -> SBool
checkBinary m = bAnd $ map (\x -> x .== 0 ||| x .== 1) $ mValues m

checkSComponent :: PetriNet -> [String] -> ModelI -> ModelSI -> SBool
checkSComponent net fired ax m =
        checkPrePostPlaces net m &&&
        checkPrePostTransitions net m &&&
        checkSubsetTransitions fired m &&&
        checkNotEmpty fired m &&&
        checkClosed net ax m &&&
        checkTokens net m &&&
        checkBinary m

checkSComponentSat :: PetriNet -> [String] -> ModelI -> ([String], ModelSI -> SBool)
checkSComponentSat net fired ax =
        (places net ++ transitions net ++ map prime fired,
         checkSComponent net fired ax)

getSComponentOutIn :: PetriNet -> ModelI -> ModelI -> ([String], [String])
getSComponentOutIn net ax as =
        partition (cElem ax) $ filter (cElem as) (transitions net)


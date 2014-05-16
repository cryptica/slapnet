module Solver.SComponent
    (checkSComponent,checkSComponentSat,
     getSComponentInOut)
where

import Data.SBV
import qualified Data.Map as M
import Data.List (partition)

import PetriNet
import Solver

mElem :: ModelSI -> String -> SBool
mElem m x = (m M.! x) .== 1

mElemI :: ModelI -> String -> Bool
mElemI m x = (m M.! x) == 1

mNotElem :: ModelSI -> String -> SBool
mNotElem m x = (m M.! x) .== 0

mNotElemI :: ModelI -> String -> Bool
mNotElemI m x = (m M.! x) == 0

countElem :: ModelSI -> [String] -> SInteger
countElem m xs = sum $ map (m M.!) xs

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
                let incoming = countElem m $ pre net t
                    outgoing = countElem m $ post net t
                in  mElem m t ==> incoming .== 1 &&& outgoing .== 1

checkSubsetTransitions :: [String] -> ModelSI -> SBool
checkSubsetTransitions fired m =
            bAnd (map checkTransition fired) &&&
            countElem m (map prime fired) .< countElem m fired
        where checkTransition t =
                mElem m (prime t) ==> mElem m t

checkNotEmpty :: [String] -> ModelSI -> SBool
checkNotEmpty fired m = countElem m (map prime fired) .> 0

checkClosed :: PetriNet -> ModelI -> ModelSI -> SBool
checkClosed net ax m =
            bAnd (map checkPlaceClosed (places net))
        where checkPlaceClosed p = mElem m p ==>
                        bAnd (map checkTransition
                             [(t,t') | t <- pre net p, t' <- post net p,
                                       ax M.! t > 0 , ax M.! t' > 0 ])
              checkTransition (t,t') =
                  mElem m (prime t) &&& mElem m t' ==> mElem m (prime t')

checkTokens :: PetriNet -> ModelSI -> SBool
checkTokens net m =
            sum (map addPlace (initials net)) .== 1
        where addPlace (p,x) = literal x * (m M.! p)

checkBinary :: ModelSI -> SBool
checkBinary m = bAnd $ map (\x -> x .== 0 ||| x .== 1) $ M.elems m

checkSComponent :: PetriNet -> [String] -> ModelI -> ModelSI -> SBool
checkSComponent net fired ax m =
        checkPrePostPlaces net m &&&
        checkPrePostTransitions net m &&&
        checkSubsetTransitions fired m &&&
        checkNotEmpty fired m &&&
        checkClosed net ax m &&&
        checkTokens net m &&&
        checkBinary m

checkSComponentSat :: PetriNet -> ModelI -> ([String], ModelSI -> SBool)
checkSComponentSat net ax =
        let fired = M.keys $ M.filter (> 0) ax
        in  (places net ++ transitions net ++ map prime fired,
             checkSComponent net fired ax)

getSComponentInOut :: PetriNet -> ModelI -> ModelI -> ([String], [String])
getSComponentInOut net ax as =
        partition (mElemI ax) $ filter (mElemI as) (transitions net)


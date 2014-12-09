module Solver.LivenessInvariant (
    checkLivenessInvariant
  , checkLivenessInvariantSat
  , getLivenessInvariant
  , PlaceVector
  , LivenessInvariant
) where

type PlaceVector [(String,Integer)]
type LivenessInvariant = [PlaceVector]

checkLivenessInvariant :: PetriNet -> [String] -> ModelI -> ModelSI -> SBool
checkLivenessInvariant net fired ax m =
        checkPrePostPlaces net m &&&
        checkPrePostTransitions net m &&&
        checkSubsetTransitions fired m &&&
        checkNotEmpty fired m &&&
        checkClosed net ax m &&&
        checkTokens net m &&&
        checkBinary m

checkLivenessInvariantSat :: PetriNet -> [String] -> ModelI ->
        ([String], ModelSI -> SBool)
checkLivenessInvariantSat net fired ax =

getLivenessInvariant :: PetriNet -> ModelI -> ModelI -> LivenessInvariant
getLivenessInvariant net ax as =

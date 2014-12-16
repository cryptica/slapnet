{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Solver
    (prime,checkSat,ModelReader,val,VarMap,
     getNames,makeVarMap,makeVarMapWith,varMapNames,
     IntConstraint,BoolConstraint,IntResult,BoolResult,
     Model(..),mVal,mValues,mElemsWith,mElemSum,SModel(..),CModel(..))
where

import Data.SBV
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Applicative

newtype Model a = Model { getMap :: M.Map String a }
newtype VarMap a = VarMap { getVarMap :: M.Map a String }

getNames :: VarMap a -> [String]
getNames = M.elems . getVarMap

instance Show a => Show (Model a) where
        show = show . M.toList . getMap

type ModelReader a b = Reader (Model a) b
type IntConstraint = ModelReader SInteger SBool
type BoolConstraint = ModelReader SBool SBool
type IntResult a = ModelReader Integer a
type BoolResult a = ModelReader Bool a

val :: (Ord a) => VarMap a -> a -> ModelReader b b
val ma x = do
        mb <- ask
        return $ getMap mb M.! (getVarMap ma M.! x)

makeVarMap :: (Show a, Ord a) => [a] -> VarMap a
makeVarMap = makeVarMapWith id

makeVarMapWith :: (Show a, Ord a) => (String -> String) -> [a] -> VarMap a
makeVarMapWith f xs = VarMap $ M.fromList $ xs `zip` map (f . show) xs

varMapNames :: VarMap a -> [String]
varMapNames = M.elems . getVarMap

prime :: String -> String
prime = ('\'':)

mVal :: Model a -> String -> a
mVal m x = M.findWithDefault (error ("key not found: " ++ x)) x (getMap m)

mValues :: Model a -> [a]
mValues m = M.elems $ getMap m

mElemsWith :: (a -> Bool) -> Model a -> [String]
mElemsWith f m = M.keys $ M.filter f $ getMap m

mElemSum :: (Num a) => Model a -> [String] -> a
mElemSum m xs = sum $ map (mVal m) xs

class SModel a where
        mElem :: Model a -> String -> SBool
        mNotElem :: Model a -> String -> SBool
        mNotElem m x = bnot $ mElem m x
class CModel a where
        cElem :: Model a -> String -> Bool
        cNotElem :: Model a -> String -> Bool
        cNotElem m x = not $ cElem m x

instance SModel SInteger where
        mElem m x = mVal m x .> 0
        mNotElem m x = mVal m x .== 0
instance SModel SBool where
        mElem = mVal
        mNotElem m x = bnot $ mVal m x
instance CModel Integer where
        cElem m x = mVal m x > 0
        cNotElem m x = mVal m x == 0
instance CModel Bool where
        cElem = mVal
        cNotElem m x = not $ mVal m x

symConstraints :: SymWord a => [String] -> ModelReader (SBV a) SBool ->
        Symbolic SBool
symConstraints vars constraint = do
        syms <- mapM exists vars
        return $ runReader constraint $ Model $ M.fromList $ vars `zip` syms

rebuildModel :: SymWord a => [String] -> Either String (Bool, [a]) ->
        Maybe (Model a)
rebuildModel _ (Left _) = Nothing
rebuildModel _ (Right (True, _)) = error "Prover returned unknown"
rebuildModel vars (Right (False, m)) = Just $ Model $ M.fromList $ vars `zip` m

checkSat :: (SatModel a, SymWord a) =>
        ([String], ModelReader (SBV a) SBool, ModelReader a b) ->
        IO (Maybe b)
checkSat (vars, constraint, interpretation) = do
        result <- satWith z3{verbose=False} $ symConstraints vars constraint
        return $ runReader interpretation <$> rebuildModel vars (getModel result)


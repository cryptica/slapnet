{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module Util
    (verbosePut,elems,items,emap,
     nubOrd,nubOrdBy,prime,val,vals,mval,zeroVal,positiveVal,sumVal,
     makeVarMap,makeVarMapWith,buildVector,makeVector,getNames,
     Vector,Model,VarMap,SIMap,SBMap,IMap,BMap)
where

import Data.SBV
import qualified Data.Map as M
import Control.Monad
import Data.List
import Data.Ord
import Data.Function

{-
- Various maps and functions on them
-}

newtype Vector a = Vector { getVector :: M.Map a Integer }
type Model a = M.Map String a
type VarMap a = M.Map a String
type SIMap a = M.Map a SInteger
type SBMap a = M.Map a SBool
type IMap a = M.Map a Integer
type BMap a = M.Map a Bool

class MapLike c a b | c -> a, c -> b where
        val :: c -> a -> b
        vals :: c -> [b]
        elems :: c -> [a]
        items :: c -> [(a,b)]

        mval :: c -> [a] -> [b]
        mval = map . val
        sumVal :: (Num b) => c -> b
        sumVal = sum . vals

instance (Ord a, Show a, Show b) => MapLike (M.Map a b) a b where
        val m x = M.findWithDefault
                    (error ("key " ++ show x ++ " not found in " ++ show m))
                    x m
        vals = M.elems
        items = M.toList
        elems = M.keys

instance (Ord a, Show a) => MapLike (Vector a) a Integer where
        val (Vector v) x = M.findWithDefault 0 x v
        vals = vals . getVector
        items = M.toList . getVector
        elems = M.keys . getVector

instance (Show a) => Show (Vector a) where
        show (Vector v) =
                "[" ++ intercalate "," (map showEntry (M.toList v)) ++ "]"
            where showEntry (i,x) =
                    show i ++ (if x /= 1 then "(" ++ show x ++ ")" else "")

emap :: (Ord a, Ord b) => (a -> b) -> Vector a -> Vector b
emap f = Vector . M.mapKeys f . getVector

zeroVal :: (Ord a, Show a) => M.Map a SInteger -> a -> SBool
zeroVal m x = val m x .== 0

positiveVal :: (Ord a, Show a) => M.Map a SInteger -> a -> SBool
positiveVal m x = val m x .> 0

makeVarMap :: (Show a, Ord a) => [a] -> VarMap a
makeVarMap = makeVarMapWith id

makeVarMapWith :: (Show a, Ord a) => (String -> String) -> [a] -> VarMap a
makeVarMapWith f xs = M.fromList $ xs `zip` map (f . show) xs

getNames :: VarMap a -> [String]
getNames = M.elems

buildVector :: (Ord a) => [(a, Integer)] -> Vector a
buildVector = makeVector . M.fromList

makeVector :: (Ord a) => M.Map a Integer -> Vector a
makeVector = Vector . M.filter (/=0)

{-
- List functions
-}

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = nubOrdBy id

nubOrdBy :: (Ord b) => (a -> b) -> [a] -> [a]
nubOrdBy f = map head . groupBy ((==) `on` f) . sortBy (comparing f)

{-
- TODO: IO wrapper with options
-}

verbosePut :: Int -> Int -> String -> IO ()
verbosePut verbosity level str =
        when (verbosity >= level) (putStrLn str)

prime :: String -> String
prime = ('\'':)



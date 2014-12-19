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

type Vector a = M.Map a Integer
type Model a = M.Map String a
type VarMap a = M.Map a String
type SIMap a = M.Map a SInteger
type SBMap a = M.Map a SBool
type IMap a = M.Map a Integer
type BMap a = M.Map a Bool

val :: (Ord a) => M.Map a b -> a -> b
val = (M.!)

mval :: (Ord a) => M.Map a b -> [a] -> [b]
mval = map . val

zeroVal :: (Ord a) => M.Map a SInteger -> a -> SBool
zeroVal m x = val m x .== 0

positiveVal :: (Ord a) => M.Map a SInteger -> a -> SBool
positiveVal m x = val m x .> 0

sumVal :: (Ord a, Num b) => M.Map a b -> b
sumVal = sum . vals

vals :: (Ord a) => M.Map a b -> [b]
vals = M.elems

elems :: (Ord a) => M.Map a b -> [a]
elems = M.keys

items :: M.Map a b -> [(a,b)]
items = M.toList

makeVarMap :: (Show a, Ord a) => [a] -> VarMap a
makeVarMap = makeVarMapWith id

makeVarMapWith :: (Show a, Ord a) => (String -> String) -> [a] -> VarMap a
makeVarMapWith f xs = M.fromList $ xs `zip` map (f . show) xs

getNames :: VarMap a -> [String]
getNames = M.elems

--instance (Show a) => Show (Vector a) where
--        show (Vector v) =
--                "[" ++ intercalate "," (map showEntry (M.toList v)) ++ "]"
--            where showEntry (i,x) =
--                    show i ++ (if x /= 1 then "(" ++ show x ++ ")" else "")

emap :: (Ord a, Ord b) => (a -> b) -> M.Map a c -> M.Map b c
emap = M.mapKeys

buildVector :: (Ord a) => [(a, Integer)] -> Vector a
buildVector = M.fromList

makeVector :: (Ord a) => M.Map a Integer -> Vector a
makeVector = M.filter (/=0)

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



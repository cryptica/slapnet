module Util
    (verbosePut,Vector,value,elems,items,buildVector,makeVector,vmap,
     nubOrd,nubOrdBy)
where

import qualified Data.Map as M
import Control.Monad
import Data.List
import Data.Ord
import Data.Function

verbosePut :: Int -> Int -> String -> IO ()
verbosePut verbosity level str =
        when (verbosity >= level) (putStrLn str)


newtype Vector a = Vector { getVector :: M.Map a Integer }

instance (Show a) => Show (Vector a) where
        show (Vector v) =
                "[" ++ intercalate "," (map showEntry (M.toList v)) ++ "]"
            where showEntry (i,x) =
                    show i ++ (if x /= 1 then "(" ++ show x ++ ")" else "")

vmap :: (Ord a, Ord b) => (a -> b) -> Vector a -> Vector b
vmap f (Vector m) = Vector $ M.mapKeys f m

value :: (Ord a) => Vector a -> a -> Integer
value v x = M.findWithDefault 0 x (getVector v)

elems :: (Ord a) => Vector a -> [a]
elems = M.keys . getVector

items :: Vector a -> [(a,Integer)]
items = M.toList . getVector

buildVector :: (Ord a) => [(a, Integer)] -> Vector a
buildVector = makeVector . M.fromList

makeVector :: (Ord a) => M.Map a Integer -> Vector a
makeVector = Vector . M.filter (/=0)

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = nubOrdBy id

nubOrdBy :: (Ord b) => (a -> b) -> [a] -> [a]
nubOrdBy f = map head . groupBy ((==) `on` f) . sortBy (comparing f)




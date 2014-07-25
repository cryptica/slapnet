{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Solver
    (checkSat,checkSatInt,checkSatBool,MModelS,MModelI,MModelB,
     MModel(..),mVal,mValues,mElemsWith,mElemSum,CModel(..),
     Z3Type(..),mkOr',mkAnd',mkAdd',mkSub',mkMul')
where

import Z3.Monad
import Control.Monad
import qualified Data.Map as M

newtype MModel a = MModel { getMap :: M.Map String a }

instance Show a => Show (MModel a) where
        show = show . M.toList . getMap

type MModelS = MModel AST
type MModelI = MModel Integer
type MModelB = MModel (Maybe Bool)

class Z3Type a where
        mkVal :: a -> Z3 AST
        getVal :: AST -> Z3 a

instance Z3Type Integer where
        mkVal = mkInt
        getVal = getInt

instance Z3Type (Maybe Bool) where
        mkVal x = case x of
                      Nothing -> error "can not make undefined constant"
                      Just True -> mkTrue
                      Just False -> mkFalse
        getVal  = getBool

mVal :: MModel a -> String -> a
mVal m x = M.findWithDefault (error ("key not found: " ++ x)) x (getMap m)

mValues :: MModel a -> [a]
mValues m = M.elems $ getMap m

mElemsWith :: (a -> Bool) -> MModel a -> [String]
mElemsWith f m = M.keys $ M.filter f $ getMap m

mElemSum :: (Num a) => MModel a -> [String] -> a
mElemSum m xs = sum $ map (mVal m) xs

mkOr' :: [AST] -> Z3 AST
mkOr' [] = mkFalse
mkOr' xs = mkOr xs

mkAnd' :: [AST] -> Z3 AST
mkAnd' [] = mkTrue
mkAnd' xs = mkAnd xs

mkAdd' :: [AST] -> Z3 AST
mkAdd' [] = mkInt (0::Integer)
mkAdd' xs = mkAdd xs

mkSub' :: [AST] -> Z3 AST
mkSub' [] = mkInt (0::Integer)
mkSub' xs = mkSub xs

mkMul' :: [AST] -> Z3 AST
mkMul' [] = mkInt (1::Integer)
mkMul' xs = mkMul xs

--class SMModel a where
--        mElem :: MModel a -> String -> Z3 AST
--        mNotElem :: MModel a -> String -> Z3 AST
--        mNotElem m x = mkNot =<< mElem m x
class CModel a where
        cElem :: MModel a -> String -> Bool
        cNotElem :: MModel a -> String -> Bool
        cNotElem m x = not $ cElem m x

--instance SMModel AST where
--        mElem m x = (mVal m x `mkGt`) =<< mkInt 0
--        mNotElem m x = mkEq (mVal m x) =<< mkInt 0
--instance SMModel AST where
--        mElem = mVal
--        mNotElem m x = mkNot $ mVal m x
instance CModel Integer where
        cElem m x = mVal m x > 0
        cNotElem m x = mVal m x == 0
instance CModel Bool where
        cElem = mVal
        cNotElem m x = not $ mVal m x

checkSat :: Z3Type a => Z3 Sort -> ([String], MModel AST -> Z3 ()) ->
        Z3 (Maybe (MModel a))
checkSat mkSort (vars, constraint) = do
        sort <- mkSort
        syms <- mapM (mkStringSymbol >=> flip mkConst sort) vars
        let smodel = MModel $ M.fromList $ vars `zip` syms
        constraint smodel
        result <- getModel
        case result of
            (Unsat, Nothing) -> return Nothing
            (Sat, Just m) -> do
                ms <- evalT m syms
                case ms of
                    Just xs -> do
                        vals <- mapM getVal xs
                        let cmodel = MModel $ M.fromList $ vars `zip` vals
                        return $ Just cmodel
                    Nothing -> error "Prover returned incomplete model"
            (Undef, _) -> error "Prover returned unknown"
            (Unsat, Just _) -> error "Prover returned unsat but a model"
            (Sat, Nothing) -> error "Prover returned sat but no model"

checkSatInt :: ([String], MModel AST -> Z3 ()) -> IO (Maybe (MModel Integer))
checkSatInt = evalZ3 . checkSat mkIntSort

checkSatBool :: ([String], MModel AST -> Z3 ()) -> IO (Maybe (MModel (Maybe Bool)))
checkSatBool = evalZ3 . checkSat mkBoolSort

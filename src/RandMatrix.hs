-----------------------------------------------------------------------------
--
-- Module      :  RandMatrix
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module RandMatrix (generateRandomMatrix

) where

import Data.Matrix as Matrix
import Data.List
import System.Random

generateRandomMatrix :: Int -> Int -> IO (Matrix Double)
generateRandomMatrix rows cols = do
        arrays <- addRow rows cols
        return (Matrix.fromLists arrays)

addRow rows cols = do
        if rows > 0
                then do
                        x <- genRandRow cols
                        xs <- addRow (rows-1) cols
                        return (x:xs)
                else return []

genRandRow cols = do
    seed <- newStdGen
    let rs =  map (/100) $ take cols (randoms seed :: [Double])
    return rs



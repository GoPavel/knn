{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Vector as V
import DataUtil (readDataN, readData)
import HyperParameter (DistanceTag(..), KernelTag(..))
import KNN (knnFindConfig, knnChart)

dataFile :: String
dataFile = "data/phpSZJq5T.csv" -- https://www.openml.org/d/1514

main :: IO ()
main = do
  (xs, ys) <- readDataN dataFile
  let maxY = 10
  -- _finalConfig <- knnFindConfig xs ys maxY
  -- _finalConfig: Uniform (Fixed 1.0e-4) Manhattan
  knnChart Uniform Manhattan xs ys maxY
  return ()

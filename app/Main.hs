{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Vector as V
import DataUtil (readDataN, readData)
import HyperParameter (DistanceTag(..), KernelTag(..))
import KNN (knnFindConfig, knnChart)

dataFile :: String
dataFile = "data/phpSZJq5T.csv" -- https://www.openml.org/d/1514

-- Ideal config:
--   Config Uniform (Fixed 10.0) Manhattan

main :: IO ()
main = do
  (xs, ys) <- readDataN dataFile
  let maxY = 10
  -- _finalConfig <- knnFindConfig xs ys maxY
  knnChart Triangular Euclidean xs ys maxY
  return ()

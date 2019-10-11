module FScore where

import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra.Data as M

import           DataUtil (weightedAverage, harm)
{-
m = [ c -- real
    , t -- answer
    ]
-}

recall, precision :: M.Matrix Double -> Int -> Double

recall m ix
  | d == 0    = 0
  | otherwise = truePositives m ix / d
  where d = truePositives m ix + falseNegatives m ix

precision m ix
  | d == 0    = 0
  | otherwise = truePositives m ix / d
  where d = truePositives m ix + falsePositives m ix

truePositives, falsePositives, falseNegatives :: M.Matrix Double -> Int -> Double

truePositives m ix = m M.! ix M.! ix

falsePositives m ix = foldr ((+) . (V.! ix)) 0 (M.toRows m) - truePositives m ix

falseNegatives m ix = V.sum (m M.! ix) - truePositives m ix

weights :: M.Matrix Double -> [Double]
weights m = map (V.sum) $ M.toRows m

microF :: M.Matrix Double -> Double
microF m
  = weightedAverage (weights m) [harm (recall m ix) (precision m ix)
                                | ix <- [0..M.rows m - 1]]

macroRecall, macroPrecision :: M.Matrix Double  -> Double
macroRecall m
  = weightedAverage (weights m) [recall m i | i <- [0..M.rows m - 1]]
macroPrecision m
  = weightedAverage (weights m) [precision m i | i <- [0..M.rows m - 1]]

macroF :: M.Matrix Double -> Double
macroF m = harm (macroPrecision m) (macroRecall m)


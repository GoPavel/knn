{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase #-}

module HyperParameter where

import qualified Data.Vector as V

data WindowConfig
  = Fixed Double
  | Variable Int
  deriving Show

type Kernel = Double -> Double
type Distance = V.Vector Double -> V.Vector Double -> Double

data DistanceTag = Manhattan | Euclidean | Chebyshev
  deriving Show
data KernelTag = Uniform | Triangular | Epanechnikov | Quartic | Triweight
  | Tricube | Gaussian | Cosine | Logistic | Sigmoid
  deriving Show

dispatchDistance :: DistanceTag -> Distance
dispatchDistance = \case
  Manhattan -> manhattan
  Euclidean -> euclidean
  Chebyshev -> chebyshev


dispatchKernel :: KernelTag -> Kernel
dispatchKernel = \case
  Uniform      -> uniform
  Triangular   -> triangular
  Epanechnikov -> epanechnikov
  Quartic      -> quartic
  Triweight    -> triweight
  Tricube      -> tricube
  Gaussian     -> gaussian
  Cosine       -> cosine
  Logistic     -> logistic
  Sigmoid      -> sigmoid


rangeDistance :: [DistanceTag]
rangeDistance = [Manhattan, Euclidean, Chebyshev]

rangeDistanceV :: V.Vector DistanceTag
rangeDistanceV = V.fromList rangeDistance

rangeKernel :: [KernelTag]
rangeKernel = [Uniform, Triangular, Epanechnikov, Quartic, Triweight, Tricube
               , Gaussian, Cosine, Logistic, Sigmoid
               ]

rangeKernelV :: V.Vector KernelTag
rangeKernelV = V.fromList rangeKernel


manhattan, euclidean, chebyshev :: Distance
-- manhattan (as, bs) = fromIntegral $ sum $ [ abs $ a - b
--                                           | (a, b) <- zip as bs]
manhattan as bs = V.sum $ V.zipWith (\a b -> abs (a - b)) as bs
euclidean as bs = sqrt $ V.sum $ V.zipWith (\a b -> (a-b)^2) as bs
chebyshev as bs = V.maximum $ V.zipWith (\ a b -> abs (a - b)) as bs

uniform, triangular, epanechnikov, quartic, triweight
  , tricube, gaussian, cosine, logistic, sigmoid :: Kernel
uniform x
  | x >= 1    = 0
  | otherwise = 0.5
triangular x
  | x >= 1    = 0
  | otherwise = (1 - abs x)
epanechnikov x
  | x >= 1    = 0
  | otherwise = 0.75 * (1 - x^2)
quartic x
  | x >= 1    = 0
  | otherwise = 15 / 16 * (1 - x^2)^2
triweight x
  | x >= 1    = 0
  | otherwise = 35 / 32 * (1 - x^2)^3
tricube x
  | x >= 1    = 0
  | otherwise = 70 / 81 * (1 - abs x^3)^3
gaussian x
  = recip (sqrt (2 * pi)) * exp (-0.5 * x^2)
cosine x
  | x >= 1    = 0
  | otherwise = pi / 4 * cos (pi / 2 * x)
logistic x
  = recip $ exp x + 2 + exp (-x)
sigmoid x
 = 2 * recip ((*pi) $ exp x + exp (-x))

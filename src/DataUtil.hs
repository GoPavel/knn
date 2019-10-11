{-# LANGUAGE ScopedTypeVariables #-}

module DataUtil
  ( readData, readDataN
  , transpose, normalize, leaveOneOut
  , mean, weightedAverage
  , argmax
  , (//), harm
  , forV, iforV, maxDist
  ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Vector as V

readData :: String -> IO (V.Vector (V.Vector Double), V.Vector Int)
readData path = do
  csvData <- BL.readFile path
  case decode NoHeader csvData of
    Left err -> error err
    Right dat -> do
      -- putStrLn "Header:"
      -- print $ dat V.! 0
      -- print $ V.map (V.last) d
      let bodyDat = V.tail dat
          numDat = forV bodyDat $ \(item :: V.Vector String) ->
            V.map read item :: V.Vector Double
          vXs = V.map V.init numDat
          vYs = V.map (round . V.last) numDat
      -- print $ V.length bodyDat
      -- print $ V.length numDat
      -- print $ V.length vYs
      -- print $ V.length vXs
      -- print $ V.length $ vXs V.! 0
      return (vXs, vYs)

readDataN :: String -> IO (V.Vector (V.Vector Double), V.Vector Int)
readDataN path = do
  (xs, ys) <- readData path
  return (normalize xs, ys)

transpose :: V.Vector (V.Vector b) -> V.Vector (V.Vector b)
transpose vec = V.generate (V.length $ V.head vec) $ \ i -> V.map (V.! i) vec

normalize :: V.Vector (V.Vector Double) -> V.Vector (V.Vector Double)
normalize dat = transpose $ V.generate (V.length $ V.head dat) norm1
  where
    norm1 i = let
      di = V.map (V.! i) dat
      minVal = V.minimum di
      maxVal = V.maximum di
      dVal = maxVal - minVal
      d = if dVal == 0 then 1 else dVal
      in V.map (\ x -> (x - minVal) / d) di

leaveOneOut :: V.Vector a -> V.Vector (V.Vector a, a)
leaveOneOut vec = V.generate (V.length vec) $
  \ i -> ((V.take i vec V.++ V.drop (i + 1) vec), vec V.! i)

weightedAverage :: [Double] -> [Double] -> Double
weightedAverage ws xs = (/ sum ws) $ sum $ zipWith (*) ws xs

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

argmax :: (Ord a, Num a) => V.Vector a -> V.Vector Int -> Int -> Int
argmax vals args maxArgs = V.maxIndex $ V.generate maxArgs $ \iCls ->
  V.sum $ V.ifilter (\ i _ -> args V.! i == iCls) $ V.takeWhile (/=0) vals

(//) :: (Integral a, Fractional b) => a -> a -> b
a // b = fromIntegral a / fromIntegral b

harm :: Double -> Double -> Double
harm 0 0 = 0
harm a b = 2 * a * b / (a + b)

forV :: V.Vector a -> (a -> b) -> V.Vector b
forV = flip V.map

iforV :: V.Vector a -> (Int -> a -> b) -> V.Vector b
iforV = flip V.imap

-- checkBound :: V.Vector a -> Int -> a -> a
-- checkBound vec ix 

maxDist :: V.Vector a -> (a -> a -> Double) -> Double
maxDist xs ro = V.maximum $
  iforV xs $ \ i xi -> V.maximum $
    iforV xs $ \j xj ->
       if i /= j then ro xi xj else 0

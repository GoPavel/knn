module Strategies where

import           Control.Parallel.Strategies
import qualified Data.Vector as V

parVector :: NFData a => Int -> V.Vector a -> V.Vector a
parVector n vec
  | n < 0     = error "n < 0"
  | otherwise = V.concat $ parMap rdeepseq (V.force) $ splitOnchunk n vec

splitOnchunk :: Int -> V.Vector a -> [V.Vector a]
splitOnchunk n vec
  | V.length vec == 0 = []
  | (chunk, rest) <- V.splitAt n vec = chunk : splitOnchunk n rest

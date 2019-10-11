{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module KNN where

import           Control.Applicative
-- import           Control.Exception (assert)
import           Control.Monad.ST
import qualified Data.Vector as V
-- import           Debug.Trace
import qualified Numeric.LinearAlgebra.Devel as DM
import           Statistics.Function (gsort)
import qualified System.ProgressBar as PB

import           Chart (drawChart)
import           DataUtil (leaveOneOut, argmax, forV, maxDist)
import           FScore (macroF)
import           HyperParameter ( WindowConfig(..), KernelTag, Kernel
                                , DistanceTag
                                , rangeKernelV, manhattan, rangeDistanceV
                                , dispatchDistance, dispatchKernel)
import           Strategies (parVector)


data Config = Config KernelTag WindowConfig DistanceTag
  deriving Show

knnFindConfig :: V.Vector (V.Vector Double)
              -> V.Vector Int
              -> Int
              -> IO (Config)
knnFindConfig xs y maxY = do
  putStrLn "Amout of instances: "
  print $ V.length xs
  putStrLn $ "Amout of feachers: "
  print $ V.length $ xs V.! 0
  putStrLn "Manhattan max distance: "
  print $ maxDist xs manhattan
  putStrLn "Dinstances: "
  print $ rangeDistanceV
  putStrLn "Kernels: "
  print $ rangeKernelV
  let configs = do
        kernT <- rangeKernelV
        wind <- Fixed <$> V.fromList
          [0.0001, 0.01, 0.1, 0.3, 0.5, 0.8, 3.0, 6.0, 10.0, 20.0, 30.0]
         <|> Variable <$> V.fromList
          [1, 3, 5, 10, 50, 100, 300, V.length xs - 2]
        roT <- rangeDistanceV
        return $ Config kernT wind roT
      cntConfigs = V.length configs
  putStrLn $ "Count of configs: " <> show cntConfigs
  pb <- PB.newProgressBar PB.defStyle 10 (PB.Progress 0 cntConfigs ())
  -- config <- return $ configs V.! 1
  -- print config
  -- print $ knnScore config maxY xs y
  scores <- V.forM configs $ \config -> do
          PB.incProgress pb 1
          return $! knnScore config xs y maxY
  -- print scores
  let indexIdealConfig = V.maxIndex $ parVector 1 scores
  putStr $ "Ideal config: #" <> show indexIdealConfig <> " with score "
  print $ scores V.! indexIdealConfig
  let resultConfig = configs V.! indexIdealConfig
  print resultConfig
  return resultConfig

knnScore :: Config
         -> V.Vector (V.Vector Double)
         -> V.Vector Int
         -> Int
         -> Double
knnScore (Config
          (dispatchKernel -> kern)
          window
          (dispatchDistance -> ro)) xs ys maxY = runST $ do
  let results = parVector 10 $ do
        -- choose split
       (train, (q, y)) <- leaveOneOut $ V.zip xs ys
       -- sort by distance
       let (dists, ys'sorted) = V.unzip $ gsort $ forV train $
                                            \(_xs, _y) -> (_xs `ro` q, _y)
           guessedY = knnCalc kern window maxY dists ys'sorted
       -- trace (show y) $ return ()
       -- trace (show q) $ return ()
       -- trace (show $ train V.! 0) $ return ()
       -- trace (show $ train V.! 1) $ return ()
       -- trace (show $ V.slice 0 15 dists) $ return ()
       -- assert (guessedY >= 0 && guessedY < maxY) $ return ()
       return (y, guessedY)
  confuseMatrix <- DM.newMatrix 0 maxY maxY :: ST s (DM.STMatrix s Double) -- TODO
  V.forM_ results $ \ (y, y') ->
    DM.modifyMatrix confuseMatrix (y-1) y' (+1)
  -- m <- DM.freezeMatrix confuseMatrix
  -- trace (show m) $ return ()
  score <- macroF <$> DM.freezeMatrix confuseMatrix
  return score

knnCalc :: Kernel
        -> WindowConfig
        -> Int
        -> V.Vector Double -- dists (sorted)
        -> V.Vector Int    -- y sorted by dists
        -> Int             -- index of guessed class
knnCalc kern window maxY ds ys
  | (Fixed h) <- window
  , weights   <- forV ds $ \ d -> kern $ d / h
  = argmax weights ys maxY
  | (Variable h) <- window
  , d'kth   <- -- assert (V.length ds > h) $
              ds V.! h
  , weights <- forV ds $ \ d -> kern $ d / d'kth
  = -- trace (show $ V.slice 0 30 weights) $
    -- trace (show $ ys) $
     argmax weights ys maxY


knnChart :: KernelTag
         -> DistanceTag
         -> V.Vector (V.Vector Double)
         -> V.Vector Int
         -> Int
         -> IO ()
knnChart kern ro xs ys maxY = do
  let rangeH = V.fromList [1, 2, 3, 5, 10, 20, 30, 50, 60, 80, 100, 150, 200, 300]
  pb <- PB.newProgressBar PB.defStyle 10 (PB.Progress 0 (V.length
                                                        rangeH) ())
  fscores <- V.forM rangeH $ \h -> do
        PB.incProgress pb 1
        let config = Config kern (Variable h) ro
            !fscore = knnScore config xs ys maxY
        return fscore
  -- print $ knnScore (Config kern (Variable 3) ro) xs ys maxY
  drawChart (V.map fromIntegral rangeH) $ parVector 1 fscores

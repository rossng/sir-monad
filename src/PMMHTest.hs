{-# LANGUAGE PartialTypeSignatures #-}
module PMMHTest where

import           Control.Monad.Bayes.Class
import           Control.Monad.Bayes.Sampler
import           Control.Monad
import           Control.Monad.Bayes.Inference.PMMH
                                               as PMMH
import           Control.Monad.Bayes.Weighted
import           Control.Monad.State
import           Numeric.Log
import           DataParser
import           Statistics
import           Utils

generateData
  :: MonadSample m
  => 
  -- | T
     Int
  ->
  -- | list of latent and observable states from t=1
     m [(Double, Double)]
generateData t = do
  (sigmaX, sigmaY) <- param
  let sq x = x * x
      simulate 0 _ acc = return acc
      simulate k x acc = do
        let n = length acc
        x' <- normal (mean x n) sigmaX
        y' <- normal (sq x' / 20) sigmaY
        simulate (k - 1) x' ((x', y') : acc)
  x0  <- normal 0 (sqrt 5)
  xys <- simulate t x0 []
  return $ reverse xys

param :: MonadSample m => m (Double, Double)
param = do
  let a = 0.01
  let b = 0.01
  precX <- gamma a b
  let sigmaX = 1 / sqrt precX
  precY <- gamma a b
  let sigmaY = 1 / sqrt precY
  return (sigmaX, sigmaY)

mean :: Double -> Int -> Double
mean x n =
  let sq x = x * x
  in  0.5 * x + 25 * x / (1 + sq x) + 8 * cos (1.2 * fromIntegral n)

model
  :: (MonadInfer m)
  => 
  -- | observed data
     [Double]
  ->
  -- | prior on the parameters
     (Double, Double)
  ->
  -- | list of latent states from t=1
     m [Double]
model obs (sigmaX, sigmaY) = do
  let sq x = x * x
      simulate []       _ acc = return acc
      simulate (y : ys) x acc = do
        let n = length acc
        x' <- normal (mean x n) sigmaX
        factor $ normalPdf (sq x' / 20) sigmaY y
        simulate ys x' (x' : acc)
  x0 <- normal 0 (sqrt 5)
  xs <- simulate obs x0 []
  return $ reverse xs


test :: IO [[([Double], Numeric.Log.Log Double)]]
test = sampleIO $ do
  let t = 5
  dat <- generateData t
  let ys = map snd dat
  pmmhRes <- prior $ pmmh 2 t 3 param (model ys)
  liftIO $ print pmmhRes
  return pmmhRes

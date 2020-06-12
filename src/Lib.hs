module Lib
    ( someFunc
    ) where

import Control.Monad.Bayes.Class
import DataParser

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Params = Params {
    rho :: Double,
    beta :: Double,
    gamma :: Double
}

data LatentState = LatentState {
    sus :: Int,
    inf :: Int,
    rec :: Int
}

{-
observation model: Poisson rho * I
-}

type InfectionCount = Int

observationModel :: MonadSample m => Params -> LatentState -> m InfectionCount
observationModel (Params rho _ _) (LatentState _ inf _) = poisson (rho * inf) 

simulateEpidemic :: MonadSample m => Params -> Int -> m Infections 
simulateEpidemic params nsteps = undefined

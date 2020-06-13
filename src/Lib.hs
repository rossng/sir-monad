module Lib
    ( someFunc
    )
where

import           Control.Monad.Bayes.Class
import           DataParser
import           Control.Monad
import           Utils

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Params = Params {
    rho :: Double, -- ^ Rate of detection
    beta :: Double, -- ^ Mean contact rate between susceptible and infected people
    gamma :: Double, -- ^ Mean recovery rate
    numPop :: Int,
    timeSlices :: Int -- ^ 1/dt
}

data LatentState = LatentState {
    sus :: Int, -- ^ Number of people susceptible to infection
    inf :: Int, -- ^ Number of people currently infected
    recov :: Int -- ^ Number of people recovered from infection
}

{-
observation model: Poisson rho * I
-}

type InfectionCount = Int

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

binomial :: MonadSample m => Int -> Double -> m Int
binomial n p = sum <$> replicateM n (boolToInt <$> bernoulli p)


observationModel :: MonadSample m => Params -> LatentState -> m InfectionCount
observationModel (Params rho _ _ _ _) (LatentState _ inf _) =
    poisson (rho * fromIntegral inf)


{-
dN_SI <-
-}
-- | Transition the model a single time slice
transitionModelSingleStep
    :: MonadSample m => Params -> LatentState -> m LatentState
transitionModelSingleStep (Params rho beta gamma numPop timeSlices) (LatentState sus inf recov)
    = do
        let dt = 1 / fromIntegral timeSlices
        dN_SI <- binomial
            sus
            (1 - exp ((-beta * dt * (fromIntegral inf)) / (fromIntegral numPop))
            )
        dN_IR <- binomial
            inf
            (1 - exp ((-beta * dt * (fromIntegral inf)) / (fromIntegral numPop))
            )
        let sus'   = sus - dN_SI
        let inf'   = inf + dN_SI - dN_IR
        let recov' = recov + dN_IR
        return (LatentState sus' inf' recov')

-- | Transition the model for a full step
transitionModel :: MonadSample m => Params -> LatentState -> m LatentState
transitionModel params =
    repeatFunction (timeSlices params) (transitionModelSingleStep params)

simulateEpidemic :: MonadSample m => Params -> Int -> m Infections
simulateEpidemic params nsteps = undefined

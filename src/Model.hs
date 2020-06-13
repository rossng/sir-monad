{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Model where

import           Control.Monad.Bayes.Class
import           Control.Monad.Bayes.Sampler
import           Control.Monad
import           Control.Monad.State

import           DataParser
import           Statistics
import           Utils

data Params = Params {
    rho :: Double, -- ^ Rate of detection
    beta :: Double, -- ^ Mean contact rate between susceptible and infected people
    gamma :: Double, -- ^ Mean recovery rate
    numPop :: Int, -- ^ Number of people
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

-- | Model for how we observe the number of infected people
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

-- | Simulate a single step, returning the new latent state and appending the observed infection count to the state.
simulateStep
    :: (MonadSample m, MonadState [InfectionCount] m)
    => Params
    -> LatentState
    -> m LatentState
simulateStep params latent = do
    latent'        <- transitionModel params latent
    infectionCount <- observationModel params latent'
    modify (++ [infectionCount])
    return latent'

-- | Simulate nsteps steps of an epidemic with the specified initial state and parameters
simulateEpidemic
    :: MonadSample m => LatentState -> Params -> Int -> m Infections
simulateEpidemic initialState params nsteps =
    Infections
        <$> execStateT
                (repeatFunction nsteps (simulateStep params) initialState)
                []

-- | Execute a single simulation of an epidemic
generateSingleEpidemic :: LatentState -> Params -> Int -> IO Infections
generateSingleEpidemic initialState params nsteps =
    sampleIO $ simulateEpidemic initialState params nsteps

params :: Params
params = Params 0.9 2.0 0.6 763 1

state :: LatentState
state = LatentState 762 1 0
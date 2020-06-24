{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import           Control.Monad.Bayes.Class
import           Control.Monad.Bayes.Sampler
import qualified Data.Text.IO as TIO
import           Control.Monad
import           Control.Monad.Bayes.Inference.PMMH
import           Control.Monad.State
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Weighted
import           Numeric.Log
import           Text.Megaparsec
import PMC

import           DataParser
import           Statistics
import           Utils


data Params = Params {
    rho :: Double, -- ^ Rate of detection
    beta :: Double, -- ^ Mean contact rate between susceptible and infected people
    gamma :: Double -- ^ Mean recovery rate
} deriving Show

data FixedParams = FixedParams {
    numPop :: Int,
    timeSlices :: Int
}

data LatentState = LatentState {
    sus :: Int, -- ^ Number of people susceptible to infection
    inf :: Int, -- ^ Number of people currently infected
    recov :: Int -- ^ Number of people recovered from infection
} deriving Show

{-
observation model: Poisson rho * I
-}


-- | Model for how we observe the number of infected people
observationModel :: MonadSample m => Params -> LatentState -> m InfectionCount
observationModel (Params rho _ _) (LatentState _ inf _) =
    poisson (rho * fromIntegral inf)


{-
dN_SI <-
-}
-- | Transition the model a single time slice
transitionModelSingleStep
    :: MonadSample m => FixedParams -> Params -> LatentState -> m LatentState
transitionModelSingleStep (FixedParams numPop timeSlices) (Params rho beta gamma) (LatentState sus inf recov)
    = do
        let dt = 1 / fromIntegral timeSlices
        dN_SI <- binomial
            sus
            (1 - exp ((-beta * dt * (fromIntegral inf)) / (fromIntegral numPop))
            )
        dN_IR <- binomial
            inf
            (1 - exp (-gamma * dt)
            )
        let sus'   = sus - dN_SI
        let inf'   = inf + dN_SI - dN_IR
        let recov' = recov + dN_IR
        return (LatentState sus' inf' recov')

-- | Transition the model for a full step
transitionModel :: MonadSample m => FixedParams -> Params -> LatentState -> m LatentState
transitionModel fixedParams params =
    transitionModelSingleStep fixedParams params

-- | Simulate a single step, returning the new latent state and appending the observed infection count to the state.
simulateStep
    :: (MonadSample m, MonadState [InfectionCount] m)
    => FixedParams
    -> Params
    -> LatentState
    -> m LatentState
simulateStep fixedParams params latent = do
    latent'        <- transitionModel fixedParams params latent
    infectionCount <- observationModel params latent'
    modify (++ [infectionCount])
    return latent'

-- | Simulate nsteps steps of an epidemic with the specified initial state and parameters
simulateEpidemic
    :: MonadSample m => LatentState -> FixedParams -> Params -> Int -> m Epidemic
simulateEpidemic initialState fixedParams params nsteps =
    Epidemic
        <$> execStateT
                (repeatFunction nsteps (simulateStep fixedParams params) initialState)
                []

-- | Execute a single simulation of an epidemic
generateSingleEpidemic :: LatentState -> FixedParams -> Params -> Int -> IO Epidemic
generateSingleEpidemic initialState fixedParams params nsteps =
    sampleIO $ simulateEpidemic initialState fixedParams params nsteps


generateEpidemics :: LatentState -> FixedParams -> Params -> Int -> Int -> IO [Epidemic]
generateEpidemics initialState fixedParams params nsteps nepidemics = replicateM nepidemics (generateSingleEpidemic initialState fixedParams params nsteps)

fixedParams :: FixedParams
fixedParams = FixedParams 763 1  

params :: Params
params = Params 0.9 2.0 0.6 

initialState :: LatentState
initialState = LatentState 762 1 0

scoreEpidemicToDatum'
    :: MonadInfer m
    => FixedParams
    -> Epidemic
    -> LatentState
    -> Params
    -> m LatentState
scoreEpidemicToDatum' fixedParams dat  x params = do
    let obs lambda y = score (poissonPdf lambda y)
        go [] x = return (head x)
        go (y:ys) xs = do
            x' <- transitionModel fixedParams params (head xs)
            obs ((fromIntegral $ inf x') * (rho params)) y
            (go ys (x' : xs))
    (go (unwrapEpidemic dat)) [x]

scoreEpidemicToDatum
    :: MonadInfer m
    => FixedParams
    -> Epidemic
    -> LatentState
    -> Params
    ->  m Params
scoreEpidemicToDatum fixedParams dat  x params = do
    let obs lambda y = score (poissonPdf lambda y)
        go [] x = return x
        go (y:ys) x = do
            x' <- transitionModel fixedParams params x
            obs ((fromIntegral $ inf x') * (rho params)) y
            go ys x'
    (go (unwrapEpidemic dat)) x
    return params

unwrapEpidemic :: Epidemic -> [Int]
unwrapEpidemic (Epidemic xs) = xs

{-
ddprior <- function(params) {
    dgamma(params[[1]], 0.3, 10, log = TRUE) +
    dgamma(params[[3]], 1, 8, log=TRUE) + 
    dbeta(params[[2]], 2,7, log=TRUE)
}
-}
paramsPrior :: MonadSample m => m Params
paramsPrior = do
    pBeta <- Control.Monad.Bayes.Class.gamma 2 3
    pRho <- Control.Monad.Bayes.Class.beta 2.0 2.0
    pGamma <- Control.Monad.Bayes.Class.gamma 2.0 0.8
    return (Params pRho pBeta pGamma )


testInferenceEpidemic nsteps nparticles = do
    ys <- parseFromFile epidemicParser "data/datafile"
    case ys of (Left _) -> error "naughty"
               (Right dat) -> sampleIO $ do
                       pmmhRes <- prior $ pmmh nsteps 14 nparticles paramsPrior (scoreEpidemicToDatum fixedParams dat initialState)
                       let (posterior, _) = unzip $ head <$> pmmhRes
                       return posterior

parseFromFile p file = runParser p file <$> TIO.readFile file

smcInferenceEpidemic' :: Int -> IO (Log Double)
smcInferenceEpidemic'  k = do
    ys <- parseFromFile epidemicParser "data/datafile"
    case ys of (Left _) -> error "naughty"
               (Right dat) -> sampleIO $ do
                                t <- evidence $ smcSystematic 14 k ((return $ Params 2 2 2) >>= (scoreEpidemicToDatum fixedParams dat initialState))
                                return t


smcInferenceEpidemic :: Int -> IO ()
smcInferenceEpidemic  k = do
    ys <- parseFromFile epidemicParser "data/datafile"
    case ys of (Left _) -> error "naughty"
               (Right dat) -> sampleIO $ do
                                t <- evidence $ smcSystematic 14 k (paramsPrior >>= (scoreEpidemicToDatum' fixedParams dat initialState))
                                liftIO $ print $ t

extractParams project samples = (project . fst . head) <$> samples

--scoreEpidemicToData :: (MonadSample m, MonadState [InfectionCount] m => Epidemic ->  Params -> LatentState -> m Epidemic 
--scoreEpidemicToData data params initialState = do
--    let obs lambda y = score (poissonPdf lambda y)
    -- simulate a new x from old x (x is a latent state)
    -- calculate new lambda
    -- score observation at time using lambda
    -- store x in the monad state
    -- repeat

generateSamples :: [Double]
generateSamples = sampleSTfixed $ replicateM 1000 (normal 3 1)





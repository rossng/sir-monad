module PMC where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Traced
import Control.Monad.Trans (lift)
import Numeric.Log

-- | Particle Marginal Metropolis-Hastings sampling.
pmc ::
  MonadInfer m =>
  -- | number of Metropolis-Hastings steps
  Int ->
  -- | number of time steps
  Int ->
  -- | number of particles
  Int ->
  -- | model parameters prior
  Traced m b ->
  -- | model
  (b -> Sequential (Population m) a) ->
  m [b]
pmc t k n param model = do
  mh t (param >>= runPopulation . pushEvidence . Pop.hoist lift . smcSystematic k n . model >>= return param)
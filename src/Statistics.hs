module Statistics where

import           Control.Monad
import           Control.Monad.Bayes.Class      ( bernoulli
                                                , MonadSample
                                                , discrete
                                                )
import           Numeric.Log
import           Statistics.Distribution        ( logProbability )
import           Statistics.Distribution.Poisson
                                                ( poisson )
import qualified Statistics.Distribution.Binomial
                                               as SB

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

binomial :: MonadSample m => Int -> Double -> m Int
--binomial n p = Prelude.sum <$> replicateM n (boolToInt <$> bernoulli p)
binomial n p = discrete $ SB.binomial n p

poissonPdf
  :: Double
  ->
  -- | lambda
     Int
  ->
  -- | x 
     Log Double
poissonPdf lambda x = Exp $ logProbability (poisson lambda) x

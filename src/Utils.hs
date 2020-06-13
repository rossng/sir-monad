module Utils where

import           Control.Monad
import           Numeric.Log
import           Statistics.Distribution.Poisson
                                                ( poisson )
import           Statistics.Distribution        ( logProbability )



repeatFunction :: Monad m => Int -> (a -> m a) -> (a -> m a)
repeatFunction n f = foldl (>=>) return (replicate n f)

{-repeatCustom :: Monad m -> Int -> (a -> m b) -> (b -> a) -> (b -> c) -> (a -> m [c])
repeatCustom n action projectInput projectOutput = foldl stitch (return []) (replicate n f)
    where stitch :: Monad m => (a -> m b) -> (a -> m b) -> (a -> m c)
          stitch l r = \x -> do
              intemediate <- l x
              return (projectOutput)
              
-}

poissonPdf
    :: Double
    ->
  -- | lambda
       Int
    ->
  -- | x 
       Log Double
poissonPdf lambda x = Exp $ logProbability (poisson lambda) x

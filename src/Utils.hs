module Utils where

import           Control.Monad

repeatFunction :: Monad m => Int -> (a -> m a) -> (a -> m a)
repeatFunction n f = foldl (>=>) return (replicate n f)

{-repeatCustom :: Monad m -> Int -> (a -> m b) -> (b -> a) -> (b -> c) -> (a -> m [c])
repeatCustom n action projectInput projectOutput = foldl stitch (return []) (replicate n f)
    where stitch :: Monad m => (a -> m b) -> (a -> m b) -> (a -> m c)
          stitch l r = \x -> do
              intemediate <- l x
              return (projectOutput)
              
-}

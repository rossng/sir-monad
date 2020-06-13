module Utils where

import           Control.Monad

repeatFunction :: Monad m => Int -> (a -> m a) -> (a -> m a)
repeatFunction n f = foldl (>=>) return (replicate n f)

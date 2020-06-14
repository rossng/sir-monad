module Main where

main :: IO ()
main = do
    x <- testInferenceEpidemic 10 10
    let x' = extractParams beta x 
    return ()

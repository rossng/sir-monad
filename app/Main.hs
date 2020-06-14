module Main where

import System.Environment

import Model
import Charting

main :: IO ()
main = do
    args <- getArgs
    x <- testInferenceEpidemic (read $ args !! 0) (read $ args !! 1)
    let bP = extractParams beta x 
    let rP = extractParams rho x 
    let gP = extractParams gamma x 
    toHtmlFile "beta.html" $ plotTrace bP
    toHtmlFile "rho.html" $ plotTrace rP
    toHtmlFile "gamma.html" $ plotTrace gP
    return ()

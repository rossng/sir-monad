module Main where

import Model
import Charting

main :: IO ()
main = do
    x <- testInferenceEpidemic 20 20
    let bP = extractParams beta x 
    let rP = extractParams rho x 
    let gP = extractParams gamma x 
    toHtmlFile "beta.html" $ plotTrace bP
    toHtmlFile "rho.html" $ plotTrace rP
    toHtmlFile "gamma.html" $ plotTrace gP
    return ()

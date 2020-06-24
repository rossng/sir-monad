module Main where

import System.Environment

import Model
import Charting
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    x <- testInferenceEpidemic (read $ args !! 0) (read $ args !! 1)
    let bP = beta <$> x
    let rP = rho <$> x
    let gP = gamma <$> x
    toHtmlFile "beta.html" $ plotTrace bP
    toHtmlFile "rho.html" $ plotTrace rP
    toHtmlFile "gamma.html" $ plotTrace gP
    toHtmlFile "dBeta.html"  $ plotDensity bP
    toHtmlFile "dRho.html"   $ plotDensity rP
    toHtmlFile "dGamma.html" $ plotDensity gP
    return ()

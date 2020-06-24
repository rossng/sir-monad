module ThirdMain where


import System.Environment

import Model
import Charting
import System.Environment



main :: IO ()
main = do
    t <- smcInferenceEpidemic' 1000
    print t
    -- let lR = (fromIntegral . recov . fst) <$> t
    -- let lI = (fromIntegral . inf . fst) <$> t
    -- toHtmlFile "recovF.html" $ plotDensity lR
    -- toHtmlFile "infF.html" $ plotDensity lI
    return ()

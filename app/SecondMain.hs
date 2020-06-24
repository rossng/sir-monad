module SecondMain where


import System.Environment

import Model
import Charting
import System.Environment

main :: IO ()
main = do
    print "hello"
    toHtmlFile "test.html" =<< (plotEpidemics <$> generateEpidemics initialState fixedParams params 100 5)

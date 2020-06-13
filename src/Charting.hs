{-# LANGUAGE OverloadedStrings #-}

module Charting
    ( module Charting
    , VL.toHtmlFile
    )
where

import qualified Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Graphics.Vega.VegaLite        as VL
import           Prelude                 hiding ( filter
                                                , lookup
                                                , repeat
                                                )

import           DataParser
import Model

dailyInfections :: Infections -> [(Int, InfectionCount)]
dailyInfections (Infections infections) = zip [1..] infections

dailyInfectionsToData :: [(Int, InfectionCount)] -> VL.Data
dailyInfectionsToData = VL.dataFromRows [] . dailyInfectionsToData'
    where dailyInfectionsToData' ((day, infections) : dailyInfections) = VL.dataRow [("NumInfected", VL.Number $ fromIntegral infections), ("Day", VL.Number $ fromIntegral day)] (dailyInfectionsToData' dailyInfections)
          dailyInfectionsToData' [] = []

infectionsToData :: Infections -> VL.Data
infectionsToData = dailyInfectionsToData . dailyInfections

plotInfections :: Infections -> VL.VegaLite
plotInfections infections = VL.toVegaLite [background, data', encoding [], plot, width, height]
    where
        plot = VL.mark VL.Circle [VL.MTooltip VL.TTEncoding]
        encoding = VL.encoding 
                        . VL.position VL.X [VL.PName "Day", VL.PmType VL.Temporal]
                        . VL.position VL.Y [VL.PName "NumInfected", VL.PmType VL.Quantitative]
        background = VL.background "rgba(0, 0, 0, 0.05)"
        width = VL.width 600
        height = VL.height 600
        data' = infectionsToData infections


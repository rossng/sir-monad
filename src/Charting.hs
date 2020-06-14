{-# LANGUAGE OverloadedStrings #-}

module Charting
    ( module Charting
    , VL.toHtmlFile
    , VL.fromVL
    , Data.Aeson.Encode.Pretty.encodePretty
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
import           Model

type Day = Int
type EpidemicDay = (Day, InfectionCount)
type Fields = [(VL.FieldName, VL.DataValue)]
type Rows = [Fields]

epidemicToDays :: Epidemic -> [EpidemicDay]
epidemicToDays (Epidemic epidemics) = zip [1 ..] epidemics

epidemicDayToFields :: EpidemicDay -> Fields
epidemicDayToFields (day, infectionCount) =
    [ ("NumInfected", VL.Number $ fromIntegral infectionCount)
    , ("Day"        , VL.Number $ fromIntegral day)
    ]

epidemicToRows :: [EpidemicDay] -> Rows
epidemicToRows = map epidemicDayToFields

concatSimulations :: [Rows] -> Rows
concatSimulations = concatMap addSimulationId . zip [1..]
    where addSimulationId :: (Int, Rows) -> Rows
          addSimulationId (i, rows) = map (("Simulation", VL.Number $ fromIntegral i) :) rows

rowsToData :: Rows -> VL.Data
rowsToData = VL.dataFromRows [] . rowsToData'
  where
    rowsToData' (fields : rest) = VL.dataRow fields (rowsToData' rest)
    rowsToData' []              = []

epidemicToData :: Epidemic -> VL.Data
epidemicToData = rowsToData . epidemicToRows . epidemicToDays

epidemicsToData :: [Epidemic] -> VL.Data
epidemicsToData = rowsToData . concatSimulations . map epidemicToRows . map epidemicToDays

plotEpidemics :: [Epidemic] -> VL.VegaLite
plotEpidemics epidemics = VL.toVegaLite
    [background, data', encoding [], plot, width, height]
  where
    plot = VL.mark VL.Line [VL.MTooltip VL.TTEncoding, VL.MInterpolate VL.Monotone]
    encoding =
        VL.encoding
            . VL.position VL.X [VL.PName "Day", VL.PmType VL.Temporal]
            . VL.position
                  VL.Y
                  [VL.PName "NumInfected", VL.PmType VL.Quantitative]
            . VL.color [ VL.MName "Simulation", VL.MmType VL.Nominal, VL.MLegend [] ]
    background = VL.background "rgba(255, 255, 255, 1.0)"
    width      = VL.width 600
    height     = VL.height 600
    data'      = epidemicsToData epidemics

plotDensity :: [Double] -> VL.VegaLite
plotDensity samples = VL.toVegaLite 
        [background, data', encoding [], plot, width, height, transform []]
    where
        background = VL.background "rgba(255, 255, 255, 1.0)"
        data' = samplesToData samples
        transform = VL.transform . VL.density "value" [ VL.DnBandwidth 0.3 ]
        encoding = VL.encoding
            . VL.position VL.X [VL.PName "value", VL.PmType VL.Quantitative]
            . VL.position VL.Y [VL.PName "density", VL.PmType VL.Quantitative]
        plot = VL.mark VL.Area [VL.MOpacity 0.7, VL.MFill "teal"]
        width      = VL.width 600
        height     = VL.height 600

plotTrace :: [Double] -> VL.VegaLite
plotTrace samples = VL.toVegaLite
    [background, data', encoding [], plot, width, height]
    where
        background = VL.background "rgba(255, 255, 255, 1.0)"
        data' = samplesToData samples
        encoding = VL.encoding
            . VL.position VL.X [VL.PName "step", VL.PmType VL.Quantitative]
            . VL.position VL.Y [VL.PName "value", VL.PmType VL.Quantitative]
        plot = VL.mark VL.Tick [VL.MOrient VL.Horizontal, VL.MWidth 5]
        width      = VL.width 600
        height     = VL.height 600

sampleToRow :: Double -> Fields
sampleToRow sample = [("value", VL.Number sample)]

samplesToRows = map sampleToRow

samplesToData :: [Double] -> VL.Data
samplesToData = rowsToData . addIncrementingStep . samplesToRows

addIncrementingStep :: Rows -> Rows
addIncrementingStep = map (\(i, row) -> ("step", VL.Number $ fromIntegral i) : row) . zip [1..]
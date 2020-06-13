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
    plot = VL.mark VL.Circle [VL.MTooltip VL.TTEncoding]
    encoding =
        VL.encoding
            . VL.position VL.X [VL.PName "Day", VL.PmType VL.Temporal]
            . VL.position
                  VL.Y
                  [VL.PName "NumInfected", VL.PmType VL.Quantitative]
            . VL.color [ VL.MName "Simulation", VL.MmType VL.Nominal, VL.MLegend [] ]
    background = VL.background "rgba(0, 0, 0, 0.05)"
    width      = VL.width 600
    height     = VL.height 600
    data'      = epidemicsToData epidemics


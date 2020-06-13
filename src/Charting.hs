{-# LANGUAGE OverloadedStrings #-}

module Charting where

import qualified Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Graphics.Vega.VegaLite        as VL
import           Prelude                 hiding ( filter
                                                , lookup
                                                , repeat
                                                )

import           DataParser

plotInfections :: Infections -> VL.VegaLite
plotInfections = undefined

chart =
    let
        cars = VL.dataFromUrl
            "https://vega.github.io/vega-datasets/data/cars.json"
            []

        enc =
            VL.encoding
                . VL.position
                      VL.X
                      [VL.PName "Horsepower", VL.PmType VL.Quantitative]
                . VL.position
                      VL.Y
                      [ VL.PName "Miles_per_Gallon"
                      , VL.PmType VL.Quantitative
                      , VL.PTitle "Miles per Gallon"
                      ]
                . VL.color [VL.MName "Origin", VL.MmType VL.Nominal]

        bkg = VL.background "rgba(0, 0, 0, 0.05)"
    in
        VL.fromVL $ VL.toVegaLite
            [bkg, cars, VL.mark VL.Circle [VL.MTooltip VL.TTEncoding], enc []]

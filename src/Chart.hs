{-# LANGUAGE OverloadedStrings #-}

module Chart where

import qualified Data.Vector as V

import Lucid as L
import Lucid.Html5 as L
import Graphics.Plotly as GP
import Graphics.Plotly.Lucid as GPL
import Lens.Micro

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

drawChart :: V.Vector Double
          -> V.Vector Double
          -> IO ()
drawChart xs ys = do
  let myTrace = line (aes & x .~ fst
                          & y .~ snd) $ V.toList $ V.zip xs ys
  T.writeFile "chart.html" $ renderText $ doctypehtml_ $ do
    head_ $ do meta_ [charset_ "uft-8"]
               plotlyCDN
    body_ $ toHtml $ plotly "Ideal Config" [myTrace]


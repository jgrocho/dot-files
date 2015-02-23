{-# LANGUAGE OverloadedStrings #-}

module Solarized
  ( red
  , magenta
  , orange
  , yellow
  , blue
  , cyan
  , green
  , violet
  ) where

import Data.String ( IsString(..) )

red, magenta, orange, yellow, blue, cyan, green, violet :: IsString a => a
red     = "#dc322f"
magenta = "#d33682"
orange  = "#cb4b16"
yellow  = "#b58900"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"
violet  = "#6c71c4"

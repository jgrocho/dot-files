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

import Data.String (IsString(..))

red     :: IsString a => a
red      = "#dc322f"

magenta :: IsString a => a
magenta  = "#d33682"

orange  :: IsString a => a
orange   = "#cb4b16"

yellow  :: IsString a => a
yellow   = "#b58900"

blue    :: IsString a => a
blue     = "#268bd2"

cyan    :: IsString a => a
cyan     = "#2aa198"

green   :: IsString a => a
green   = "#859900"

violet  :: IsString a => a
violet   = "#6c71c4"

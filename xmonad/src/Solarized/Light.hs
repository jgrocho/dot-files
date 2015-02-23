{-# LANGUAGE OverloadedStrings #-}

module Solarized.Light
  ( base03
  , base02
  , base01
  , base00
  , base0
  , base1
  , base2
  , base3
  ) where

import Data.String ( IsString(..) )

base03, base02, base01, base00, base0, base1, base2, base3 :: IsString a => a
base3  = "#002b36"
base2  = "#073642"
base1  = "#586e75"
base0  = "#657b83"
base00 = "#839496"
base01 = "#93a1a1"
base02 = "#eee8d5"
base03 = "#fdf6e3"

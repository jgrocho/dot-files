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

import Data.String (IsString(..))

base3   :: IsString a => a
base3    = "#002b36"

base2   :: IsString a => a
base2    = "#073642"

base1   :: IsString a => a
base1    = "#586e75"

base0   :: IsString a => a
base0    = "#657b83"

base00  :: IsString a => a
base00   = "#839496"

base01  :: IsString a => a
base01   = "#93a1a1"

base02  :: IsString a => a
base02   = "#eee8d5"

base03  :: IsString a => a
base03   = "#fdf6e3"

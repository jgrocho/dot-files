{-# LANGUAGE OverloadedStrings #-}

module Solarized.Dark
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

base03  :: IsString a => a
base03   = "#002b36"

base02  :: IsString a => a
base02   = "#073642"

base01  :: IsString a => a
base01   = "#586e75"

base00  :: IsString a => a
base00   = "#657b83"

base0   :: IsString a => a
base0    = "#839496"

base1   :: IsString a => a
base1    = "#93a1a1"

base2   :: IsString a => a
base2    = "#eee8d5"

base3   :: IsString a => a
base3    = "#fdf6e3"

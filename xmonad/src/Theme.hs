{-# LANGUAGE OverloadedStrings #-}

module Theme
  ( background
  , backgroundSecondary
  , backgroundHighlight

  , foreground
  , foregroundSeconday
  , foregroundHighlight

  , border
  , borderSecondary
  , borderHighlight

  , active  , activeText  , activeBorder
  , inactive, inactiveText, inactiveBorder
  , urgent  , urgentText  , urgentBorder

  , red
  , magenta
  , orange
  , yellow
  , blue
  , cyan
  , green
  , violet

  , good
  , bad

  , coldest
  , cold
  , tepid
  , hot
  , hottest

  , font
  , atSize
  , normalFont

  ) where

import           Data.Monoid
import           Data.String
import           Solarized
import qualified Solarized.Dark as Dark

background          :: IsString a => a
background          = Dark.base03
backgroundSecondary :: IsString a => a
backgroundSecondary = Dark.base03
backgroundHighlight :: IsString a => a
backgroundHighlight = Dark.base02

foreground           :: IsString a => a
foreground           = Dark.base0
foregroundSeconday   :: IsString a => a
foregroundSeconday   = Dark.base01
foregroundHighlight  :: IsString a => a
foregroundHighlight  = Dark.base1

border          :: IsString a => a
border          = Dark.base1
borderSecondary :: IsString a => a
borderSecondary = Dark.base02
borderHighlight :: IsString a => a
borderHighlight = Dark.base2

active         :: IsString a => a
active         = background
activeText     :: IsString a => a
activeText     = foreground
activeBorder   :: IsString a => a
activeBorder   = border

inactive       :: IsString a => a
inactive       = backgroundSecondary
inactiveText   :: IsString a => a
inactiveText   = foregroundSeconday
inactiveBorder :: IsString a => a
inactiveBorder = borderSecondary

urgent         :: IsString a => a
urgent         = background
urgentText     :: IsString a => a
urgentText     = red
urgentBorder   :: IsString a => a
urgentBorder   = borderHighlight

good :: IsString a => a
good = green
bad  :: IsString a => a
bad  = red

coldest :: IsString a => a
coldest = blue
cold    :: IsString a => a
cold    = cyan
tepid   :: IsString a => a
tepid   = green
hot     :: IsString a => a
hot     = orange
hottest :: IsString a => a
hottest = red

font :: IsString a => a
font = "xft:inconsolata"

atSize :: (Monoid a, IsString a) => a -> Double -> a
atSize font = mappend (font <> ":size=") . fromString . show

normalFont :: (Monoid a, IsString a) => a
normalFont = font `atSize` 11

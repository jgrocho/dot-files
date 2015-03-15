{-# LANGUAGE OverloadedStrings #-}

module Theme
  ( background
  , backgroundSecondary
  , backgroundHighlight

  , foreground
  , foregroundSecondary
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

import           Data.Monoid     ( Monoid(mappend), (<>) )
import           Data.String     ( IsString(..) )
import           Solarized
import qualified Solarized.Light as Light

background, backgroundSecondary, backgroundHighlight :: IsString a => a
background          = Light.base03
backgroundSecondary = Light.base03
backgroundHighlight = Light.base02

foreground, foregroundSecondary, foregroundHighlight :: IsString a => a
foreground          = Light.base0
foregroundSecondary = Light.base01
foregroundHighlight = Light.base1

border, borderSecondary, borderHighlight :: IsString a => a
border          = Light.base1
borderSecondary = Light.base02
borderHighlight = Light.base2

active, activeText, activeBorder :: IsString a => a
active         = background
activeText     = foreground
activeBorder   = border

inactive, inactiveText, inactiveBorder :: IsString a => a
inactive       = backgroundSecondary
inactiveText   = foregroundSecondary
inactiveBorder = borderSecondary

urgent, urgentText, urgentBorder :: IsString a => a
urgent         = background
urgentText     = red
urgentBorder   = borderHighlight

good, bad :: IsString a => a
good = green
bad  = red

coldest, cold, tepid, hot, hottest :: IsString a => a
coldest = blue
cold    = cyan
tepid   = green
hot     = orange
hottest = red

font :: IsString a => a
font = "xft:inconsolata"

atSize :: (Monoid a, IsString a) => a -> Double -> a
atSize fnt = mappend (fnt <> ":size=") . fromString . show

normalFont :: (Monoid a, IsString a) => a
normalFont = font `atSize` 11

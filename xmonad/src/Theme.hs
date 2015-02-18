module Theme
  ( background
  , backgroundHighlight

  , foreground
  , foregroundSeconday
  , foregroundEmphasized
  , foregroundHighlight

  , border

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

  ) where

import           Data.String    (IsString(..))
import           Solarized
import qualified Solarized.Dark as Dark

background          :: IsString a => a
background          = Dark.base03
backgroundHighlight :: IsString a => a
backgroundHighlight = Dark.base02

foreground           :: IsString a => a
foreground           = Dark.base0
foregroundSeconday   :: IsString a => a
foregroundSeconday   = Dark.base01
foregroundEmphasized :: IsString a => a
foregroundEmphasized = Dark.base1
foregroundHighlight  :: IsString a => a
foregroundHighlight  = Dark.base1

border :: IsString a => a
border = Dark.base2

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

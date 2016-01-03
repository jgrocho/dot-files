{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module XMobarHs
  ( Config   (..)
  , Position (..)
  , Align    (..)
  , Border   (..)
  , Command  (..)
  , Run      (..)
  , color
  , surround
  , wrap
  , config
  , export
  , exportTo
  ) where

import Prelude   hiding ( print, writeFile )
import Data.List        ( foldl' )
import Data.Monoid      ( Monoid(mappend, mempty), (<>), mconcat )
import Data.String      ( IsString(..) )
import Data.Text        ( Text, intercalate )
import Data.Text.IO     ( writeFile )
import GHC.Generics     ( Generic, Generic1 )
import System.Directory ( createDirectoryIfMissing, getHomeDirectory )
import System.FilePath  ( (</>), takeDirectory )
import TextShow         ( TextShow(showbPrec), showt )
import TextShow.Generic ( genericShowbPrec )

class ToText a where
    text :: a -> Text

instance ToText Text where
    text = surround "\""

instance ToText Int where
    text = showt

instance ToText Bool where
    text = showt

instance ToText a => ToText [a] where
    text = wrap "[" "]" . intercalate "," . map text

instance (ToText a, ToText b) => ToText (a, b) where
    text (a, b) = wrap "(" ")" $ intercalate "," xs
      where xs = [text a, text b]

instance (ToText a, ToText b, ToText c) => ToText (a, b, c) where
    text (a, b, c) = wrap "(" ")" $ intercalate "," xs
      where xs = [text a, text b, text c]

data Config =
    Config { font             :: Text
           , additionalFonts  :: [Text]
           , bgColor          :: Text
           , fgColor          :: Text
           , alpha            :: Int
           , position         :: Position
           , textOffset       :: Int
           , iconOffset       :: Int
           , lowerOnStart     :: Bool
           , hideOnStart      :: Bool
           , allDesktops      :: Bool
           , overrideRedirect :: Bool
           , pickBroadest     :: Bool
           , persistent       :: Bool
           , border           :: Border
           , borderColor      :: Text
           , borderWidth      :: Int
           , iconRoot         :: Text
           , commands         :: [Run Command]
           , sepChar          :: Text
           , alignSep         :: Text
           , template         :: Text
           }

instance ToText Config where
    text n = "Config{" <> e n <> "}"
      where e x = intercalate "," $ map g $ h x
            g x = fst x <> "=" <> snd x
            h x = map (\y -> (fst y, snd y x)) cfgPairs

cfgPairs :: [(Text, Config -> Text)]
cfgPairs =
    [ ( "font"             , text . font             )
    , ( "additionalFonts"  , text . additionalFonts  )
    , ( "bgColor"          , text . bgColor          )
    , ( "fgColor"          , text . fgColor          )
    , ( "alpha"            , text . alpha            )
    , ( "position"         , text . position         )
    , ( "textOffset"       , text . textOffset       )
    , ( "iconOffset"       , text . iconOffset       )
    , ( "lowerOnStart"     , text . lowerOnStart     )
    , ( "hideOnStart"      , text . hideOnStart      )
    , ( "allDesktops"      , text . allDesktops      )
    , ( "overrideRedirect" , text . overrideRedirect )
    , ( "pickBroadest"     , text . pickBroadest     )
    , ( "persistent"       , text . persistent       )
    , ( "border"           , text . border           )
    , ( "borderColor"      , text . borderColor      )
    , ( "borderWidth"      , text . borderWidth      )
    , ( "iconRoot"         , text . iconRoot         )
    , ( "commands"         , text . commands         )
    , ( "sepChar"          , text . sepChar          )
    , ( "alignSep"         , text . alignSep         )
    , ( "template"         , text . template         )
    ]

config :: Config
config =
    Config { font             = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
           , additionalFonts  = []
           , bgColor          = "#000000"
           , fgColor          = "#BFBFBF"
           , alpha            = 255
           , position         = Top
           , textOffset       = -1
           , iconOffset       = -1
           , lowerOnStart     = True
           , hideOnStart      = False
           , allDesktops      = True
           , overrideRedirect = True
           , pickBroadest     = False
           , persistent       = False
           , border           = NoBorder
           , borderColor      = "#BFBFBF"
           , borderWidth      = 1
           , iconRoot         = "."
           , commands         = [ Run $ Date "%a %b %_d %Y * %H:%M:%S" "theDate" 10
                                , Run StdinReader]
           , sepChar          = "%"
           , alignSep         = "}{"
           , template         = "%StdinReader% }{ <fc=#00FF00>%uname%</fc> * <fc=#FF0000>%theDate%</fc>"
           }

data Position = Top    | TopP    Int Int | TopW    Align Int | TopSize    Align Int Int
              | Bottom | BottomP Int Int | BottomW Align Int | BottomSize Align Int Int
              | Static { xpos   :: Int
                       , ypos   :: Int
                       , width  :: Int
                       , height :: Int
                       }
              deriving Generic

instance TextShow Position where
    showbPrec = genericShowbPrec

instance ToText Position where
    text = showt

data Align = L | C | R deriving Generic

instance TextShow Align where
    showbPrec = genericShowbPrec

data Border = TopB    | TopBM    Int
            | BottomB | BottomBM Int
            | FullB   | FullBM   Int
            | NoBorder
            deriving Generic

instance TextShow Border where
    showbPrec = genericShowbPrec

instance ToText Border where
    text = showt

data Command = Uptime                                  [Text] Int
             | Weather            Text                 [Text] Int
             | Network            Text                 [Text] Int
             | DynNetwork                              [Text] Int
             | Wireless           Text                 [Text] Int
             | Memory                                  [Text] Int
             | Swap                                    [Text] Int
             | Cpu                                     [Text] Int
             | MultiCpu                                [Text] Int
             | Battery                                 [Text] Int
             | BatteryP           [Text]               [Text] Int
             | BatteryN           [Text]               [Text] Int Text
             | TopProc                                 [Text] Int
             | TopMem                                  [Text] Int
             | DiskU              [(Text, Text)]       [Text] Int
             | DiskIO             [Text]               [Text] Int
             | ThermalZone        Int                  [Text] Int
             | CpuFreq                                 [Text] Int
             | CoreTemp                                [Text] Int
             | Volume             Text   Text          [Text] Int
             | MPD                                     [Text] Int
             | AutoMPD                                 [Text]
             | Mpris1             Text                 [Text] Int
             | Mpris2             Text                 [Text] Int
             | Mail                                    [Text]     Text
             | MBox               [(Text, Text, Text)] [Text]     Text
             | XPropertyLog       Text
             | UnsafeXPropertyLog Text
             | NamedXPropertyLog  Text                            Text
             | Brightness                              [Text] Int
             | Kbd                [(Text, Text)]
             | Locks
             | CatInt             Int Text             [Text] Int
             | Fan                Text                 [Text] Int
             | Com                Text [Text] Text            Int
             | StdinReader
             | UnsafeStdinReader
             | Date               Text Text Int
             | DateZone           Text Text Text Text Int
             | CommandReader      Text Text
             | PipeReader         Text Text
             | MarqueePipeReader  Text (Int, Int, Text) Text
             | BufferedPipeReader Text [(Int, Bool, Text)]
             | XMonadLog
             | UnsafeXMonadLog
             deriving Generic

instance ToText Command where
    text cmd = case cmd of
           (Uptime args rate)                    -> textData "Uptime" args rate
           (Weather station args rate)           -> textData "Weather" station args rate
           (Network iface args rate)             -> textData "Network" iface args rate
           (DynNetwork args rate)                -> textData "DynNetwork" args rate
           (Wireless iface args rate)            -> textData "Wireless" iface args rate
           (Memory args rate)                    -> textData "Memory" args rate
           (Swap args rate)                      -> textData "Swap" args rate
           (Cpu args rate)                       -> textData "Cpu" args rate
           (MultiCpu args rate)                  -> textData "MultiCpu" args rate
           (Battery args rate)                   -> textData "Battery" args rate
           (BatteryP dirs args rate)             -> textData "BatteryP" dirs args rate
           (BatteryN dirs args rate alias)       -> textData "BatteryN" dirs args rate alias
           (TopProc args rate)                   -> textData "TopProc" args rate
           (TopMem args rate)                    -> textData "TopMem" args rate
           (DiskU disks args rate)               -> textData "DiskU" disks args rate
           (DiskIO disks args rate)              -> textData "DiskIO" disks args rate
           (ThermalZone n args rate)             -> textData "ThermalZone" n args rate
           (CpuFreq args rate)                   -> textData "CpuFreq" args rate
           (CoreTemp args rate)                  -> textData "CoreTemp" args rate
           (Volume mixer element args rate)      -> textData "Volume" mixer element args rate
           (MPD args rate)                       -> textData "MPD" args rate
           (AutoMPD args)                        -> textData "AutoMPD" args
           (Mpris1 name args rate)               -> textData "Mpris1" name args rate
           (Mpris2 name args rate)               -> textData "Mpris2" name args rate
           (Mail args alias)                     -> textData "Mail" args alias
           (MBox boxes opts alias)               -> textData "MBox" boxes opts alias
           (XPropertyLog prop)                   -> textData "XPropertyLog" prop
           (UnsafeXPropertyLog prop)             -> textData "UnsafeXPropertyLog" prop
           (NamedXPropertyLog prop alias)        -> textData "NamedXPropertyLog" prop alias
           (Brightness args rate)                -> textData "Brightness" args rate
           (Kbd opts)                            -> textData "KBD" opts
           (Locks)                               -> textData "Locks"
           (CatInt n fn args rate)               -> textData "CatInt" n fn args rate
           (Fan path args rate)                  -> textData "Fan" path args rate
           (Com prog args alias rate)            -> textData "Com" prog args alias rate
           (StdinReader)                         -> textData "StdinReader"
           (UnsafeStdinReader)                   -> textData "UnsafeStdinReader"
           (Date fmt alias rate)                 -> textData "Date" fmt alias rate
           (DateZone fmt locale zone alias rate) -> textData "DateZone" fmt locale zone alias rate
           (CommandReader prog alias)            -> textData "CommandReader" prog alias
           (PipeReader pipe alias)               -> textData "PipeReader" pipe alias
           (MarqueePipeReader pipe opts alias)   -> textData "MarqueePipeReader" pipe opts alias
           (BufferedPipeReader alias pipes)      -> textData "BufferedPipeReader" alias pipes
           (XMonadLog)                           -> textData "XMonadLog"
           (UnsafeXMonadLog)                     -> textData "UnsafeXMonadLog"

textData :: TextDataType r => Text -> r
textData name = textData' name []

class TextDataType r where
    textData' :: Text -> [Text] -> r

instance TextDataType Text where
    textData' name acc = intercalate " " $ name : acc

instance (ToText a, TextDataType r) => TextDataType (a -> r) where
    textData' name acc = \x -> textData' name (acc ++ [text x])

data Run a = Run a

instance ToText a => ToText (Run a) where
    text (Run x) = "Run " <> text x

color :: (Monoid a, IsString a, Eq a) => a -> a -> a -> a
color fc bc content = mconcat ["<fc=", fc, if mempty == bc then "" else "," <> bc, ">", content, "</fc>"]

wrap :: Monoid a => a -> a -> a -> a
wrap l r m = l <> m <> r

surround :: Monoid a => a -> a -> a
surround o m = wrap o o m

export :: Config -> IO ()
export cfg = getHomeDirectory >>= exportTo cfg . (</> ".xmobarrc")

exportTo :: Config -> FilePath -> IO ()
exportTo cfg file = do
    createDirectoryIfMissing True $ takeDirectory file
    writeFile file $ text cfg

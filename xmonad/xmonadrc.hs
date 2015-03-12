module Main where

import XMonad                       ( Dimension, Event, mod4Mask, xmonad )
import XMonad.Config                ( defaultConfig )
import XMonad.Core                  ( ManageHook, WorkspaceId, X )
import qualified XMonad.Core as XC  ( XConfig(..) )
import XMonad.ManageHook            ( (-->), (<+>), (=?), className, composeAll, doShift )
import XMonad.Operations            ( sendMessage, windows, withFocused )
import XMonad.StackSet              ( greedyView, shift )

import XMonad.Actions.Search        ( SearchEngine(SearchEngine), (!>), alpha, dictionary, google, hoogle, images, imdb, intelligent, maps, mathworld, namedEngine, prefixAware, searchEngine, selectSearchBrowser, thesaurus, use, wayback, wikipedia, wiktionary, youtube )
import XMonad.Hooks.DynamicLog      ( PP(..), defaultPP, dynamicLogWithPP, shorten, xmobarColor )
import XMonad.Hooks.ManageDocks     ( ToggleStruts(..), avoidStruts, docksEventHook, manageDocks )
import XMonad.Hooks.ManageHelpers   ( doFullFloat, isFullscreen )
import XMonad.Hooks.UrgencyHook     ( NoUrgencyHook(..), withUrgencyHook )
import XMonad.Layout                ( Full(..), Mirror(..), Tall(..), (|||) )
import XMonad.Layout.Accordion      ( Accordion(..) )
import XMonad.Layout.Decoration     ( Theme(..), defaultTheme, shrinkText )
import XMonad.Layout.Fullscreen     ( fullscreenFull )
import XMonad.Layout.Grid           ( Grid(..) )
import XMonad.Layout.IM             ( withIM )
import XMonad.Layout.Minimize       ( MinimizeMsg(RestoreNextMinimizedWin), minimize, minimizeWindow )
import XMonad.Layout.NoBorders      ( noBorders, smartBorders )
import XMonad.Layout.PerWorkspace   ( onWorkspace )
import XMonad.Layout.Tabbed         ( tabbed )
import XMonad.Util.Dmenu            ( menuArgs )
import XMonad.Util.EZConfig         ( additionalKeysP )
import XMonad.Util.Run              ( safeSpawn, spawnPipe )
import XMonad.Util.WindowProperties ( Property(..) )
import XMonad.Util.XSelection       ( safePromptSelection )

import System.IO                    ( Handle, hPutStrLn )
import Control.Monad                ( when )
import Data.Monoid                  ( All )
import Data.Ratio                   ( (%) )

import           Theme (atSize)
import qualified Theme as Theme

-- Define the default terminal.
terminal :: String
terminal = "urxvtc"

-- Define the width of borders
borderWidth :: Dimension
borderWidth = 1

-- Define number and names of workspaces.
-- First come named layouts, which include a number.
-- The rest are just numbered.
workspaces :: [WorkspaceId]
workspaces = named ++ map show [(length named +1)..9]
  where
    names = ["main", "web", "chat", "music", "games"]
    named = zipWith (\x -> ((show x ++) "❘" ++)) [1..] names

-- Define the layout.
-- We have vertical and horizontal Tall layouts, an Accordion layout and a
-- tabbed layout, that allow room for xmobar instances.
-- We also have a fullscreen layout that covers the xmobars.
-- On workspace 3, we have a Grid-ed IM layout, which places the buddy list
-- window on the far left.
layoutHook =
    minimize $ smartBorders $
    onWorkspace (workspaces !! pred 3) chatLayout $
    onWorkspace (workspaces !! pred 5) gameLayout $
    avoidStruts
        (   tiled
        ||| Mirror tiled
        ||| Accordion
        ||| tabbedLayout
        ) |||
    fullscreenFull Full
  where
    tiled = Tall nmaster delta ratio
    -- Default number of windows in the master pane
    nmaster = 1
    -- Percent of screen to increment when resizing
    delta = 3/100
    -- Default proption of screen for the master pane
    ratio = 1/2

    tabbedLayout = tabbed shrinkText solarizedTheme
        { fontName   = Theme.font `atSize` 8
        , decoHeight = 18
        }

    chatLayout = avoidStruts $ withIM (1%7) pidginRoster Grid
    pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"

    gameLayout = noBorders $ Full

    solarizedTheme = defaultTheme
        { activeColor         = Theme.active
        , activeTextColor     = Theme.activeText
        , activeBorderColor   = Theme.activeBorder
        , inactiveColor       = Theme.inactive
        , inactiveTextColor   = Theme.inactiveText
        , inactiveBorderColor = Theme.inactiveBorder
        , urgentColor         = Theme.urgent
        , urgentTextColor     = Theme.urgentText
        , urgentBorderColor   = Theme.urgentBorder
        }

-- Define the Manage hook.
-- Always send Firefox and Chromium to the "web" workspace, Spotify to the
-- music workspace, and Skype and Pidgin to the chat workspace.
manageHook :: ManageHook
manageHook = composeAll
    ([isFullscreen --> doFullFloat] ++ classMappings ++ [manageDocks]) <+> XC.manageHook defaultConfig
  where
    classMappings = concat $
        map (\(workspace, names) -> [className =? name --> doShift (workspaces !! (workspace-1)) | name <- names])
            [ (2, [ "Firefox"
                  , "Chromium"
                  , "Google-chrome-stable"
                  , "Uzbl-core"
                  ])
            , (3, [ "Skype"
                  , "Pidgin"
                  ])
            , (4, [ "Spotify"
                  ])
            , (5, [ "net-minecraft-MinecraftLauncher"
                  , "net-ftb-gui-LaunchFrame"
                  , "net-ftb-mclauncher-MinecraftLauncher"
                  , "com-atlauncher-App"
                  , "MultiMC5"
                  ])
            ]

-- Define the Log hook.
-- Configures xmobar.
logHook :: Handle -> X ()
logHook xmobar = dynamicLogWithPP
    defaultPP { ppCurrent = xmobarColor Theme.foregroundHighlight Theme.backgroundHighlight
              , ppVisible = xmobarColor Theme.activeText Theme.active
              , ppHidden  = xmobarColor Theme.inactiveText Theme.inactive
              , ppUrgent  = xmobarColor Theme.urgentText Theme.urgent . (:) '!'
              , ppSep     = xmobarColor Theme.foregroundSecondary "" " ║ "
              , ppWsSep   = xmobarColor Theme.foregroundSecondary "" "│"
              , ppTitle   = xmobarColor Theme.foregroundHighlight "" . shorten 50
              , ppLayout  = const ""
              , ppOutput  = hPutStrLn xmobar
              }

handleEventHook :: Event -> X All
handleEventHook = XC.handleEventHook defaultConfig <+> docksEventHook

keys :: [(String, X ())]
keys = [ ("M-b"  , sendMessage ToggleStruts)
       , ("M-x"  , withFocused minimizeWindow)
       , ("M-S-x", sendMessage RestoreNextMinimizedWin)
       , ("M-s"  , searchMulti)
       , ("M-S-s", selectSearchBrowser "xdg-open" google)
       ]
       ++ [ ("M-C-S-" ++ k, (windows $ shift i) >> (windows $ greedyView i))
              | (i, k) <- zip workspaces $ map show [1..9] ]
       ++ [ ("M-i " ++ key, action) | (key, action) <- prefixActions ]
       ++ [ ("M-p " ++ key, safeSpawn program []) | (key, program) <- programList ]
  where prefixActions =
            [ ("d", safeSpawn "xdotool" ["mousedown", "1"])
            , ("f", safeSpawn "xdotool" ["mousedown", "3"])
            , ("e", safeSpawn "xdotool" ["mouseup", "1"])
            , ("r", safeSpawn "xdotool" ["mouseup", "3"])
            , ("s", safeSpawn "xset"    ["dpms", "force", "off"])
            , ("o", safePromptSelection "xdg-open")
            ]
        programList =
            [ ("p", "dmenu_run")
            , ("t", terminal)
            , ("u", "uzbl-browser")
            , ("f", "firefox")
            , ("e", "emacs")
            , ("v", "vlc")
            , ("s", "spotify")
            ]
        searchMulti = do
            let names = [name ++ ":" | SearchEngine name _ <- searchList]
            query <- menuArgs "dmenu" ["-p", "search"] names
            when (query /= "") $ safeSpawn "xdg-open" [use multiEngine query]
        amazon        = searchEngine "amazon"        "https://www.amazon.com/s?field-keywords="
        aur           = searchEngine "aur"           "https://aur.archlinux.org/packages.php?O=0&K="
        ddg           = searchEngine "ddg"           "https://duckduckgo.com/?t=lm&q="
        genius        = searchEngine "genius"        "http://genius.com/search?q="
        github        = searchEngine "github"        "https://github.com/search?type=Everything&start_value=1&q="
        hackage       = searchEngine "hackage"       "http://hackage.haskell.org/packages/search?terms="
        mdn           = searchEngine "mdn"           "https://developer.mozilla.org/en-US/search?q="
        openstreetmap = searchEngine "openstreetmap" "http://www.openstreetmap.org/search?query="
        soundcloud    = searchEngine "soundcloud"    "https://soundcloud.com/search?q="
        urban         = searchEngine "urban"         "http://www.urbandictionary.com/define.php?term="
        searchList    = [alpha, amazon, aur, ddg, dictionary, genius, github, google, hackage, hoogle, images, imdb, maps, mathworld, mdn, openstreetmap, soundcloud, thesaurus, urban, wayback, wikipedia, wiktionary, youtube]
        multiEngine   = namedEngine "multi" $ intelligent $ foldr1 (!>) $ searchList ++ [prefixAware ddg]

multimediaKeys :: [(String, X ())]
multimediaKeys = [ (audioKey "Play"          , safeSpawn "mpc" ["toggle"])
                 , (audioKey "Stop"          , safeSpawn "mpc" ["stop"])
                 , (audioKey "Prev"          , safeSpawn "mpc" ["prev"])
                 , (audioKey "Next"          , safeSpawn "mpc" ["next"])
                 , (audioKey "Mute"          , safeSpawn "amixer" ["set", "Master", "toggle"])
                 , (audioKey "LowerVolume"   , safeSpawn "amixer" ["set", "Master", "unmute"]
                                                 >> safeSpawn "amixer" ["set", "Master", "256-"])
                 , (audioKey "RaiseVolume"   , safeSpawn "amixer" ["set", "Master", "unmute"]
                                                 >> safeSpawn "amixer" ["set", "Master", "256+"])
                 , (mediaKey "Display"       , safeSpawn "display_switch" [])
                 , (mediaKey "TouchpadToggle", safeSpawn "mouse_switch" [])
                 , (mediaKey "ScreenSaver"   , safeSpawn "lock" [])
                 , (mediaKey "Battery"       , safeSpawn "sudo" ["ignore-lid"])
                 , (mediaKey "WebCam"        , safeSpawn "sudo" ["fan-switch"])
                 ]
  where mediaKey k = "<XF86" ++ k ++ ">"
        audioKey k = mediaKey $ "Audio" ++ k

main :: IO ()
main = do
    xmobar0 <- spawnPipe "xmobar -x 0"
    xmobar1 <- spawnPipe "xmobar -x 1"
    safeSpawn "xmobar" [".music_xmobarrc"]
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { XC.terminal           = terminal
        , XC.focusedBorderColor = Theme.border
        , XC.normalBorderColor  = Theme.borderSecondary
        , XC.borderWidth        = borderWidth
        , XC.modMask            = mod4Mask
        , XC.workspaces         = workspaces
        , XC.layoutHook         = layoutHook
        , XC.manageHook         = manageHook
        , XC.logHook            = logHook xmobar0 >> logHook xmobar1
        , XC.handleEventHook    = handleEventHook
        , XC.focusFollowsMouse  = False
        , XC.clickJustFocuses   = False
        }
        `additionalKeysP`
            (keys ++ multimediaKeys)

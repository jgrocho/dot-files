module Main where

import XMonad                       ( Dimension, Event, mod4Mask, xmonad )
import XMonad.Config                ( defaultConfig )
import XMonad.Core                  ( ManageHook, WorkspaceId, X, XConfig(..), handleEventHook, spawn )
import XMonad.ManageHook            ( (-->), (<+>), (=?), className, composeAll, doShift )
import XMonad.Operations            ( sendMessage, withFocused )

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
myTerminal :: String
myTerminal = "urxvtc"

-- Define the width of borders
myBorderWidth :: Dimension
myBorderWidth = 1

-- Define number and names of workspaces.
-- First come named layouts, which include a number.
-- The rest are just numbered.
myWorkspaces :: [WorkspaceId]
myWorkspaces = named ++ map show [(length named +1)..9]
  where
    names = ["main", "web", "chat", "music", "games"]
    named = zipWith (\x -> ((show x ++) "❘" ++)) [1..] names

-- Define the layout.
-- We have vertical and horizontal Tall layouts, an Accordion layout and a
-- tabbed layout, that allow room for xmobar instances.
-- We also have a fullscreen layout that covers the xmobars.
-- On workspace 3, we have a Grid-ed IM layout, which places the buddy list
-- window on the far left.
myLayout =
    minimize $ smartBorders $
    onWorkspace (myWorkspaces !! pred 3) chatLayout $
    onWorkspace (myWorkspaces !! pred 5) gameLayout $
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
myManageHook :: ManageHook
myManageHook = composeAll
    ([isFullscreen --> doFullFloat] ++ classMappings ++ [manageDocks]) <+> manageHook defaultConfig
  where
    classMappings = concat $
        map (\(workspace, names) -> [className =? name --> doShift (myWorkspaces !! (workspace-1)) | name <- names])
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
myLogHook :: Handle -> X ()
myLogHook xmobar = dynamicLogWithPP
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

myEventHook :: Event -> X All
myEventHook = handleEventHook defaultConfig <+> docksEventHook

myKeys :: [(String, X ())]
myKeys = [ ("M-b", sendMessage ToggleStruts)
         , ("M-x", withFocused minimizeWindow)
         , ("M-S-x", sendMessage RestoreNextMinimizedWin)
         , ("M-s", searchMulti)
         , ("M-S-s", selectSearchBrowser "xdg-open" google)
         ]
         ++ [ ("M-i " ++ key, action) | (key, action) <- prefixActions ]
         ++ [ ("M-p " ++ key, spawn program) | (key, program) <- programList ]
  where prefixActions =
            [ ("d", spawn "xdotool mousedown 1")
            , ("f", spawn "xdotool mousedown 3")
            , ("e", spawn "xdotool mouseup 1")
            , ("r", spawn "xdotool mouseup 3")
            , ("o", safePromptSelection "xdg-open")
            , ("s", spawn "xset dpms force off")
            ]
        programList =
            [ ("p", "dmenu_run")
            , ("t", myTerminal)
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
        multiEngine   = namedEngine "multi" $ intelligent $ foldr1 (!>) $ searchList ++ [prefixAware ddg]
        amazon        = searchEngine "amazon" "https://www.amazon.com/s?field-keywords="
        aur           = searchEngine "aur" "https://aur.archlinux.org/packages.php?O=0&K="
        ddg           = searchEngine "ddg" "https://duckduckgo.com/?t=lm&q="
        genius        = searchEngine "genius" "http://genius.com/search?q="
        github        = searchEngine "github" "https://github.com/search?type=Everything&start_value=1&q="
        hackage       = searchEngine "hackage" "http://hackage.haskell.org/packages/search?terms="
        mdn           = searchEngine "mdn" "https://developer.mozilla.org/en-US/search?q="
        openstreetmap = searchEngine "openstreetmap" "http://www.openstreetmap.org/search?query="
        soundcloud    = searchEngine "soundcloud" "https://soundcloud.com/search?q="
        urban         = searchEngine "urban" "http://www.urbandictionary.com/define.php?term="
        searchList    = [alpha, amazon, aur, ddg, dictionary, genius, github, google, hackage, hoogle, images, imdb, maps, mathworld, mdn, openstreetmap, soundcloud, thesaurus, urban, wayback, wikipedia, wiktionary, youtube]

multimediaKeys :: [(String, X ())]
multimediaKeys = [ ("<XF86AudioPlay>", spawn "mpc toggle")
                 , ("<XF86AudioStop>", spawn "mpc stop")
                 , ("<XF86AudioPrev>", spawn "mpc prev")
                 , ("<XF86AudioNext>", spawn "mpc next")
                 , ("<XF86AudioMute>", spawn "amixer set Master toggle")
                 , ("<XF86AudioLowerVolume>", spawn "amixer set Master unmute; amixer set Master 256-")
                 , ("<XF86AudioRaiseVolume>", spawn "amixer set Master unmute; amixer set Master 256+")
                 , ("<XF86Display>", spawn "display_switch")
                 , ("<XF86TouchpadToggle>", spawn "mouse_switch")
                 , ("<XF86ScreenSaver>", spawn "lock")
                 , ("<XF86Battery>", spawn "sudo ignore-lid")
                 , ("<XF86WebCam>", spawn "sudo fan-switch")
                 ]

main :: IO ()
main = do
    xmobar0 <- spawnPipe "xmobar -x 0"
    xmobar1 <- spawnPipe "xmobar -x 1"
    safeSpawn "xmobar" [".music_xmobarrc"]
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal = myTerminal
        , normalBorderColor = Theme.borderSecondary
        , focusedBorderColor = Theme.border
        , borderWidth = myBorderWidth
        , modMask = mod4Mask
        , workspaces = myWorkspaces
        , layoutHook = myLayout
        , manageHook = myManageHook
        , logHook = myLogHook xmobar0 >> myLogHook xmobar1
        , handleEventHook = myEventHook
        , focusFollowsMouse = False
        }
        `additionalKeysP`
            multimediaKeys
        `additionalKeysP`
            myKeys

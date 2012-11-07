import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Accordion
import XMonad.Layout.Tabbed
import XMonad.Layout.Minimize
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig

import System.IO

import qualified Solarized.Dark as S

-- Define the default terminal.
myTerminal = "urxvtc"

-- Define the border color for non focused windows
myNormalBorderColor = S.base02

-- Define the border color for the focused window
myFocuedBorderColor = S.violet

-- Define the width of borders
myBorderWidth = 1

-- Define number and names of workspaces.
-- First come named layouts, which include a number.
-- The rest are just numbered.
myWorkspaces = named ++ map show [(length named +1)..9]
  where
    names = ["main", "web", "irc", "music"]
    named = zipWith (\x -> ((show x ++) ":" ++)) [1..] names

-- Define the layout.
-- Adds an Accordion layout and smartBorders
-- Adds dock support to the default.
myLayout = minimize $ smartBorders $ avoidStruts (
    tiled |||
    Mirror tiled |||
    Accordion |||
    tabbed shrinkText solarizedTheme
        { fontName   = "xft:inconsolata:size=8"
        , decoHeight = 18
        }) |||
    fullscreenFull Full
  where
    tiled = Tall nmaster delta ratio
    -- Default number of windows in the master pane
    nmaster = 1
    -- Percent of screen to increment when resizing
    delta = 3/100
    -- Default proption of screen for the master pane
    ratio = 1/2

    solarizedTheme = defaultTheme
        { activeColor         = S.base03
        , inactiveColor       = S.base02
        , urgentColor         = S.base02
        , activeBorderColor   = S.base0
        , inactiveBorderColor = S.base01
        , urgentBorderColor   = S.red
        , activeTextColor     = S.base0
        , inactiveTextColor   = S.base01
        , urgentTextColor     = S.orange
        }

-- Define the Manage hook.
-- Always send Firefox and Chromium to the "web" workspace, and Spotify
-- to the music workspace.
-- Adds dock support to the default.
--myManageHook = manageDocks <+> manageHook defaultConfig
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Firefox"  --> doShift "2:web"
    , className =? "Chromium" --> doShift "2:web"
    , className =? "Spotify"  --> doShift "4:music"
    , manageDocks
    ] <+> manageHook defaultConfig

-- Define the Log hook.
-- Configures xmobar.
myLogHook xmobar = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn xmobar
    , ppCurrent = xmobarColor S.blue ""
    , ppTitle = xmobarColor S.green "" . shorten 50
    , ppLayout = const ""
    }

myEventHook = handleEventHook defaultConfig <+> docksEventHook

myKeys = [ ("M-b", sendMessage ToggleStruts)
         , ("M-x", withFocused minimizeWindow)
         , ("M-S-x", sendMessage RestoreNextMinimizedWin)
         ]

multimediaKeys = [ ("<XF86AudioPlay>", spawn "mpc toggle")
                 , ("<XF86AudioStop>", spawn "mpc stop")
                 , ("<XF86AudioPrev>", spawn "mpc prev")
                 , ("<XF86AudioNext>", spawn "mpc next")
                 , ("<XF86AudioMute>", spawn "amixer set Master toggle")
                 , ("<XF86AudioLowerVolume>", spawn "amixer set Master unmute; amixer set Master 1-")
                 , ("<XF86AudioRaiseVolume>", spawn "amixer set Master unmute; amixer set Master 1+")
                 ]

main = do
    xmobar0 <- spawnPipe "xmobar -x 0"
    xmobar1 <- spawnPipe "xmobar -x 1"
    xmonad $ defaultConfig
        { terminal = myTerminal
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocuedBorderColor
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

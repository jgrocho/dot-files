module Main where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Accordion
import XMonad.Layout.Tabbed
import XMonad.Layout.Minimize
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.XSelection

import System.IO
import Data.Ratio ((%))

import qualified Solarized.Dark as Theme

-- Define the default terminal.
myTerminal = "urxvtc"

-- Define the border color for non focused windows
myNormalBorderColor = Theme.base02

-- Define the border color for the focused window
myFocuedBorderColor = Theme.violet

-- Define the width of borders
myBorderWidth = 1

-- Define number and names of workspaces.
-- First come named layouts, which include a number.
-- The rest are just numbered.
myWorkspaces = named ++ map show [(length named +1)..9]
  where
    names = ["main", "web", "chat", "music", "games"]
    named = zipWith (\x -> ((show x ++) ":" ++)) [1..] names

-- Define the layout.
-- We have vertical and horizontal Tall layouts, an Accordion layout and a
-- tabbed layout, that allow room for xmobar instances.
-- We also have a fullscreen layout that covers the xmobars.
-- On workspace 3, we have a Grid-ed IM layout, which places the buddy list
-- window on the far left.
myLayout =
    minimize $ smartBorders $
    onWorkspace (myWorkspaces !! (3-1)) chatLayout $
    onWorkspace (myWorkspaces !! (5-1)) gameLayout $
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
        { fontName   = "xft:inconsolata:size=8"
        , decoHeight = 18
        }

    chatLayout = avoidStruts $ withIM (1%7) pidginRoster Grid
    pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"

    gameLayout = noBorders $ Full

    solarizedTheme = defaultTheme
        { activeColor         = Theme.base03
        , inactiveColor       = Theme.base02
        , urgentColor         = Theme.base02
        , activeBorderColor   = Theme.base0
        , inactiveBorderColor = Theme.base01
        , urgentBorderColor   = Theme.red
        , activeTextColor     = Theme.base0
        , inactiveTextColor   = Theme.base01
        , urgentTextColor     = Theme.orange
        }

-- Define the Manage hook.
-- Always send Firefox and Chromium to the "web" workspace, Spotify to the
-- music workspace, and Skype and Pidgin to the chat workspace.
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
myLogHook xmobar = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn xmobar
    , ppCurrent = xmobarColor Theme.blue ""
    , ppTitle = xmobarColor Theme.green "" . shorten 50
    , ppLayout = const ""
    }

myEventHook = handleEventHook defaultConfig <+> docksEventHook

myKeys = [ ("M-b", sendMessage ToggleStruts)
         , ("M-x", withFocused minimizeWindow)
         , ("M-S-x", sendMessage RestoreNextMinimizedWin)
         , (prefix "d", spawn "xdotool mousedown 1")
         , (prefix "f", spawn "xdotool mousedown 3")
         , (prefix "e", spawn "xdotool mouseup 1")
         , (prefix "r", spawn "xdotool mouseup 3")
         , (prefix "o", safePromptSelection "xdg-open")
         , (prefix "s", spawn "xset dpms force off")
         , (programPrefix "p", spawn "dmenu_run")
         , (programPrefix "t", spawn myTerminal)
         , (programPrefix "u", spawn "uzbl-browser")
         , (programPrefix "f", spawn "firefox")
         , (programPrefix "e", spawn "emacs")
         , (programPrefix "v", spawn "vlc")
         ]
  where
    prefix = let p = "M-i " in (p ++)
    programPrefix = let p = "M-p " in (p ++)

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
        , logHook = myLogHook xmobar0 >> myLogHook xmobar1 -- <+> ewmhDesktopsLogHook
        , handleEventHook = myEventHook
        , focusFollowsMouse = False
        , startupHook = setWMName "LG3D"
        }
        `additionalKeysP`
            multimediaKeys
        `additionalKeysP`
            myKeys

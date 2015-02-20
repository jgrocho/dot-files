module Main where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
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

import           Theme    (atSize)
import qualified Theme as Theme

-- Define the default terminal.
myTerminal = "urxvtc"

-- Define the width of borders
myBorderWidth = 1

-- Define number and names of workspaces.
-- First come named layouts, which include a number.
-- The rest are just numbered.
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
myLogHook xmobar = dynamicLogWithPP
    defaultPP { ppCurrent = xmobarColor Theme.foregroundHighlight Theme.backgroundHighlight
              , ppVisible = xmobarColor Theme.activeText Theme.active
              , ppHidden  = xmobarColor Theme.inactiveText Theme.inactive
              , ppUrgent  = xmobarColor Theme.urgentText Theme.urgent . (:) '!'
              , ppSep     = xmobarColor Theme.foregroundSeconday "" " ║ "
              , ppWsSep   = xmobarColor Theme.foregroundSeconday "" "│"
              , ppTitle   = xmobarColor Theme.foregroundHighlight "" . shorten 50
              , ppLayout  = const ""
              , ppOutput  = hPutStrLn xmobar
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
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal = myTerminal
        , normalBorderColor = Theme.borderSecondary
        , focusedBorderColor = Theme.border
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

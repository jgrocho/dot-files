import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig
import System.IO

-- Define the default terminal.
myTerminal = "urxvtc"

-- Define the border color for non focused windows
myNormalBorderColor = "#073642"

-- Define the border color for the focused window
myFocuedBorderColor = "#dc322f"

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
-- Adds dock support to the default.
myLayout = avoidStruts $ layoutHook defaultConfig

-- Define the Manage hook.
-- Always send Firefox and Chromium to the "web" workspace, and Spotify
-- to the music workspace.
-- Adds dock support to the default.
--myManageHook = manageDocks <+> manageHook defaultConfig
myManageHook = composeAll
    [ className =? "Firefox"  --> doShift "2:web"
    , className =? "Chromium" --> doShift "2:web"
    , className =? "Spotify"  --> doShift "4:music"
    , manageDocks
    ] <+> manageHook defaultConfig

-- Define the Log hook.
-- Configures xmobar.
myLogHook xmobar = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn xmobar
    , ppTitle = xmobarColor "#859900" "" . shorten 50
    }

myEventHook = handleEventHook defaultConfig <+> docksEventHook

multimediaKeys = [ ("<XF86AudioPlay>", spawn "mpc toggle")
                 , ("<XF86AudioStop>", spawn "mpc stop")
                 , ("<XF86AudioPrev>", spawn "mpc prev")
                 , ("<XF86AudioNext>", spawn "mpc next")
                 , ("<XF86AudioMute>", spawn "amixer set PCM toggle")
                 , ("<XF86AudioLowerVolume>", spawn "amixer set PCM 1-")
                 , ("<XF86AudioRaiseVolume>", spawn "amixer set PCM 1+")
                 ]

main = do
    xmobar <- spawnPipe "xmobar"
    xmobar_mpd <- spawnPipe "xmobar ~/.xmobarrc_mpd"
    xmonad $ defaultConfig
        { terminal = myTerminal
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocuedBorderColor
        , borderWidth = myBorderWidth
        , modMask = mod4Mask
        , workspaces = myWorkspaces
        , layoutHook = myLayout
        , manageHook = myManageHook
        , logHook = myLogHook xmobar
        , handleEventHook = myEventHook
        }
        `additionalKeysP`
            multimediaKeys

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
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
-- Adds dock suppor to the default.
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
myLogHook xmproc = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor "#859900" "" . shorten 50
    }

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { terminal = myTerminal
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocuedBorderColor
        , borderWidth = myBorderWidth
        , workspaces = myWorkspaces
        , layoutHook = myLayout
        , manageHook = myManageHook
        , logHook = myLogHook xmproc
        }

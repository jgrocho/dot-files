import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import System.IO

-- Define the default terminal.
myTerminal = "urxvt"

-- Define number and names of workspaces.
-- First come named layouts, which include a number.
-- The rest are just numbered.
myWorkspaces = named ++ map show [(length named +1)..9]
  where
    names = ["main"]
    named = zipWith (\x -> ((show x ++) ":" ++)) [1..] names

-- Define the layout.
-- Adds dock suppor to the default.
myLayout = avoidStruts $ layoutHook defaultConfig

-- Define the Manage hook.
-- Adds dock support to the default.
myManageHook = manageDocks <+> manageHook defaultConfig

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
        , workspaces = myWorkspaces
        , layoutHook = myLayout
        , manageHook = myManageHook
        , logHook = myLogHook xmproc
        }

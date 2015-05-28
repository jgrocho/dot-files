module Main where

import XMonad                         ( Dimension, Event, mod4Mask, xmonad )
import XMonad.Core                    ( ManageHook, Message, WorkspaceId, X, io, withDisplay, withWindowSet )
import qualified XMonad.Core as XC    ( XConfig(..) )
import XMonad.ManageHook              ( (-->), (<+>), (=?), className, composeAll, doShift, resource )
import XMonad.Operations              ( getCleanedScreenInfo, sendMessage, sendMessageWithNoRefresh, windows, withFocused )
import qualified XMonad.StackSet as W
import XMonad.StackSet                ( RationalRect(..), greedyView, shift )

import XMonad.Actions.Search          ( SearchEngine(SearchEngine), (!>), alpha, dictionary, google, hoogle, images, imdb, intelligent, maps, mathworld, namedEngine, prefixAware, searchEngine, selectSearchBrowser, thesaurus, use, wayback, wikipedia, wiktionary )
import XMonad.Hooks.DynamicLog        ( PP(..), dynamicLogWithPP, shorten, xmobarColor )
import XMonad.Hooks.ManageDocks       ( ToggleStruts(..), avoidStruts, docksEventHook, manageDocks )
import XMonad.Hooks.ManageHelpers     ( doFullFloat, isFullscreen )
import XMonad.Hooks.UrgencyHook       ( NoUrgencyHook(..), withUrgencyHook )
import XMonad.Layout                  ( Full(..), Mirror(..), Tall(..) )
import XMonad.Layout.Accordion        ( Accordion(..) )
import XMonad.Layout.Decoration       ( Theme(..), shrinkText )
import XMonad.Layout.Fullscreen       ( fullscreenFull )
import XMonad.Layout.Grid             ( Grid(..) )
import XMonad.Layout.IM               ( withIM )
import XMonad.Layout.LayoutCombinators( (|||), JumpToLayout(..) )
import XMonad.Layout.Minimize         ( MinimizeMsg(RestoreNextMinimizedWin), minimize, minimizeWindow )
import XMonad.Layout.NoBorders        ( noBorders, smartBorders )
import XMonad.Layout.PerWorkspace     ( onWorkspace )
import XMonad.Layout.Renamed          ( Rename(..), renamed )
import XMonad.Layout.Tabbed           ( tabbed )
import XMonad.Util.Dmenu              ( menuArgs )
import XMonad.Util.EZConfig           ( additionalKeysP )
import XMonad.Util.NamedScratchpad    ( NamedScratchpad(NS), customFloating, defaultFloating, namedScratchpadAction, namedScratchpadFilterOutWorkspacePP, namedScratchpadManageHook )
import XMonad.Util.Run                ( safeSpawn, spawnPipe )
import XMonad.Util.WindowProperties   ( Property(..) )
import XMonad.Util.XSelection         ( safePromptSelection )

import Control.Monad                  ( forM_, mapM, mapM_, when )
import Data.Default                   ( def )
import Data.List                      ( intercalate )
import Data.Maybe                     ( fromMaybe )
import Data.Monoid                    ( All )
import Data.Ratio                     ( (%) )
import Network.HostName               ( getHostName )
import System.IO                      ( Handle, hPutStrLn )
import System.Environment.XDG.BaseDir ( getUserDataFile )

import           SpawnNamedPipes ( getNamedPipes, spawnNamedPipes )
import           Theme           ( atSize )
import qualified Theme as Theme
import           XMobar          ( bottomConfig, topConfig )
import           XMobarHs        ( exportTo, wrap )

xmobarPipePrefix :: String
xmobarPipePrefix = "xmobar"

xmobarConfigs :: [String]
xmobarConfigs = ["top", "bottom"]

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
    names = ["main", "web", "chat", "music", "full"]
    named = zipWith (\x -> ((show x ++) "❘" ++)) [1..] names

selectWorkspace :: Int -> WorkspaceId
selectWorkspace = (!!) workspaces . pred

-- Define the layout.
-- We have vertical and horizontal Tall layouts, an Accordion layout and a
-- tabbed layout, that allow room for xmobar instances.
-- We also have a fullscreen layout that covers the xmobars.
-- On workspace 3, we have a Grid-ed IM layout, which places the buddy list
-- window on the far left.
layoutHook =
    renamed [CutWordsLeft 1] $
    minimize $ smartBorders $
    onWorkspace (selectWorkspace 3) chatLayout $
    onWorkspace (selectWorkspace 5) gameLayout $
    avoidStruts
        (   tiled
        ||| (renamed [Replace "Wide"] $ Mirror tiled)
        ||| Accordion
        ||| (renamed [Replace "Tabbed"] $ tabbedLayout)
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

    solarizedTheme = def
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
    ([isFullscreen --> doFullFloat] ++ classMappings ++ [manageDocks]) <+> namedScratchpadManageHook scratchpads <+> XC.manageHook def
  where classMappings = concatMap (uncurry shiftNames) $ zip [0..] classNames
        shiftNames workspace names = [className =? name --> doShift (workspaces !! workspace) | name <- names]
        classNames =
            [ []
            , [ "Firefox"
              , "Chromium"
              , "Google-chrome-stable"
              , "Uzbl-core"
              ]
            , [ "Skype"
              , "Pidgin"
              ]
            , [ "Spotify"
              ]
            , [ "net-minecraft-MinecraftLauncher"
              , "net-ftb-gui-LaunchFrame"
              , "net-ftb-mclauncher-MinecraftLauncher"
              , "com-atlauncher-App"
              , "MultiMC5"
              ]
            ]

-- Define the Log hook.
-- Configures xmobar.
logHook :: X ()
logHook = do
    let names = zipWith (++) (repeat xmobarPipePrefix) xmobarConfigs
    handles <- mapM getNamedPipes names
    sequence_ $ zipWith (\pp hs -> mapM_ pp $ fromMaybe [] hs) [topPP, botPP] handles
  where topPP handle = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $
            def { ppCurrent = xmobarColor Theme.foregroundHighlight Theme.backgroundHighlight
                , ppVisible = xmobarColor Theme.activeText Theme.active . clickableWs
                , ppHidden  = xmobarColor Theme.inactiveText Theme.inactive . clickableWs
                , ppUrgent  = xmobarColor Theme.urgentText Theme.urgent . (:) '!' . clickableWs
                , ppSep     = xmobarColor Theme.foregroundSecondary "" " ║ "
                , ppWsSep   = xmobarColor Theme.foregroundSecondary "" "│"
                , ppLayout  = wrap "<action=`xdotool key super+space`>" "</action>" . raw
                , ppTitle   = const ""
                , ppOutput  = hPutStrLn handle
                }
        botPP handle = dynamicLogWithPP $
            def { ppCurrent = const ""
                , ppVisible = const ""
                , ppHidden  = const ""
                , ppUrgent  = const ""
                , ppSep     = ""
                , ppWsSep   = ""
                , ppTitle   = xmobarColor Theme.foregroundHighlight ""
                , ppLayout  = const ""
                , ppOutput  = hPutStrLn handle
                }
        raw str = let len = show $ length str
                  in concat ["<raw=", len, ":", str, "/>"]
        clickableWs ws = let i = [head ws]
                         in concat ["<action=`xdotool key super+", i, "`>", raw ws, "</action>"]

handleEventHook :: Event -> X All
handleEventHook = XC.handleEventHook def <+> docksEventHook

scratchpads :: [ NamedScratchpad ]
scratchpads = [ NS "terminal" spawnTerm  findTerm  manageTerm
              , NS "mixer"    spawnMixer findMixer manageMixer
              ]
  where spawnTerm   = terminal ++ " -name scratchpad"
        findTerm    = resource =? "scratchpad"
        manageTerm  = customFloating $ RationalRect l t w h
          where h = 25 % 100
                w = 1
                t = 0
                l = (1 - w) / 2
        spawnMixer  = "pavucontrol"
        findMixer   = className =? "Pavucontrol"
        manageMixer = customFloating $ RationalRect l t w h
          where h = 6 % 10
                w = 6 % 10
                t = (1 - h) / 2
                l = (1 - w) / 2

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
       ++ [ ("M-n " ++ key, namedScratchpadAction scratchpads name) | (key, name) <- scratchpadList ]
       ++ [ ("M-r " ++ show i, safeSpawn "pkill" ["-f", "-USR1", "redshift.*" ++ show i]) | i <- [0..1] ]
  where prefixActions =
            [ ("d", safeSpawn "xdotool" ["mousedown", "1"])
            , ("f", safeSpawn "xdotool" ["mousedown", "3"])
            , ("e", safeSpawn "xdotool" ["mouseup", "1"])
            , ("r", safeSpawn "xdotool" ["mouseup", "3"])
            , ("s", safeSpawn "xset"    ["dpms", "force", "off"])
            , ("o", safePromptSelection "xdg-open")
            ]
        programList =
            [ ("c", "google-chrome-stable")
            , ("e", "emacs")
            , ("f", "firefox")
            , ("n", "dmenu_netctl")
            , ("p", "dmenu_run")
            , ("s", "spotify")
            , ("t", terminal)
            , ("u", "uzbl-browser")
            , ("v", "vlc")
            ]
        scratchpadList =
            [ ("t", "terminal")
            , ("m", "mixer")
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
        isup          = searchEngine "isup"          "http://www.downforeveryoneorjustme.com/"
        mdn           = searchEngine "mdn"           "https://developer.mozilla.org/en-US/search?q="
        openstreetmap = searchEngine "openstreetmap" "http://www.openstreetmap.org/search?query="
        soundcloud    = searchEngine "soundcloud"    "https://soundcloud.com/search?q="
        urban         = searchEngine "urban"         "http://www.urbandictionary.com/define.php?term="
        youtube       = searchEngine "youtube"       "https://www.youtube.com/results?search_query="
        searchList    = [alpha, amazon, aur, ddg, dictionary, genius, github, google, hackage, hoogle, images, imdb, isup, maps, mathworld, mdn, openstreetmap, soundcloud, thesaurus, urban, wayback, wikipedia, wiktionary, youtube]
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
                 ++ [ ("<XF86Launch1> " ++ key, safeSpawn "sudo" ["systemctl", action]) | (key, action) <- powerKeys ]
  where mediaKey k = "<XF86" ++ k ++ ">"
        audioKey k = mediaKey $ "Audio" ++ k
        powerKeys =
            [ ("h", "hibernate")
            , ("p", "poweroff" )
            , ("r", "reboot"   )
            , ("s", "suspend"  )
            ]

sendMessageToWorkspace :: Message a => a -> WorkspaceId -> X ()
sendMessageToWorkspace a i =
    withWindowSet $ \ws -> do
        let c = W.workspace . W.current $ ws
            v = map W.workspace . W.visible $ ws
            h = W.hidden ws
            o = head $ filter (\w -> W.tag w == i) (c : v ++ h)
        sendMessageWithNoRefresh a o

startupHook :: X ()
startupHook = do
    sendMessageToWorkspace (JumpToLayout "Tabbed") $ selectWorkspace 2

    hostname <- io getHostName
    paths    <- io $ mapM (getUserDataFile "xmobar") xmobarConfigs
    screens  <- fmap length $ withDisplay getCleanedScreenInfo
    io $ mapM_ (uncurry exportTo) $ zip [topConfig hostname, bottomConfig hostname] paths
    let spawnPipes (cfg, path) = spawnNamedPipes [command screen path | screen <- [0..screens-1]] $ xmobarPipePrefix ++ cfg
    mapM_ spawnPipes $ zip xmobarConfigs paths
  where command screen path = intercalate " " ["xmobar",  "-x", show screen, path]

config = def { XC.terminal           = terminal
             , XC.focusedBorderColor = Theme.border
             , XC.normalBorderColor  = Theme.borderSecondary
             , XC.borderWidth        = borderWidth
             , XC.modMask            = mod4Mask
             , XC.workspaces         = workspaces
             , XC.layoutHook         = layoutHook
             , XC.manageHook         = manageHook
             , XC.logHook            = logHook
             , XC.handleEventHook    = handleEventHook
             , XC.startupHook        = startupHook
             , XC.focusFollowsMouse  = False
             , XC.clickJustFocuses   = False
             }
             `additionalKeysP`
                 (keys ++ multimediaKeys)

main :: IO ()
main = xmonad $ withUrgencyHook NoUrgencyHook $ config

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Monoid      ( (<>) )
import qualified Data.Text        as T
import           Network.HostName ( HostName, getHostName )
import qualified Theme            as Theme
import           XMobarHs

data Host =
    Host { hostCores   :: Int
         , hostCpus    :: Int
         , hostBattery :: Bool
         , hostTemp    :: Bool
         , hostFan     :: Bool
         }

hosts :: [(HostName, Host)]
hosts = [ (""      , Host 1 1 False False False )
        , ("qubert", Host 2 4 True  True  True  )
        , ("randy" , Host 1 2 False False False )
        ]

lookupHost :: HostName -> Host
lookupHost host = maybe (snd $ hosts!!0) id $ lookup host hosts

interface :: T.Text
interface = "wifi0"

dateFormat :: T.Text
dateFormat = "%a %-d %b %-H:%M"

alias :: T.Text -> T.Text
alias = surround $ sepChar sharedConfig

primarySeparator :: T.Text
primarySeparator = xmobarColor Theme.foregroundSecondary " │ "

secondarySeparator :: T.Text
secondarySeparator = xmobarColor Theme.foregroundSecondary " ∙ "

sharedConfig :: Config
sharedConfig =
    config { font = Theme.normalFont
           , bgColor = Theme.background
           , fgColor = Theme.foreground
           , position = Top
           , commands = [ Run $ Memory ["-t", "<usedratio>%"] 10
                        , Run $ Date dateFormat "date" 10
                        , Run $ Network interface ["-t", "↓<rx> ↑<tx>", "-S", "False"] 10
                        , Run $ Wireless interface ["-t", "<essid> <quality>%"] 10
                        , Run $ StdinReader
                        ]
           }

multiCpuTemplate :: Host -> T.Text
multiCpuTemplate host = T.intercalate " " $ map ((<> "%") . wrap "<" ">" . ("total" <>) . T.pack . show) [0..cpus-1]
  where cpus = hostCpus host

coreTempTemplate :: Host -> T.Text
coreTempTemplate host = T.intercalate " " $ map ((<> "C") . wrap "<" ">" . ("core" <>) . T.pack . show) [1..cores]
  where cores = hostCores host

hostTemplate :: Host -> T.Text
hostTemplate host =
    T.intercalate (alignSep sharedConfig)
        [ alias "StdinReader"
        , T.intercalate primarySeparator $ filter (/= T.empty)
            [ T.intercalate secondarySeparator $ map alias [interface <> "wi", interface]
            , T.intercalate secondarySeparator $ map alias $ ["multicpu"]
                                                             ++ (if hostTemp host then ["coretemp"] else [])
                                                             ++ (if hostFan host then ["cat0"] else [])
            , (if hostBattery host then alias "battery" else "") <> alias "memory"
            , xmobarColor Theme.cyan $ alias "date"
            ]
        ]

myConfig :: HostName -> Config
myConfig hostname = let host = lookupHost hostname in
    sharedConfig { commands = [ Run $ MultiCpu ["-L", "3", "-H", "50", "--normal", Theme.good, "--high", Theme.bad, "-t", multiCpuTemplate host] 10 ]
                              ++ (if hostBattery host
                                     then [ Run $ BatteryP ["BAT0"] ["-t", "<acstatus>", "-l", Theme.bad, "-h", Theme.good, "--", "-O", "↑<left>%" <> primarySeparator, "-o", "↓<left>%" <> primarySeparator, "-i", ""] 20 ]
                                     else [])
                              ++ (if hostTemp host
                                     then [ Run $ CoreTemp ["-L", "40", "-H", "60", "-l", Theme.coldest, "-h", Theme.hottest, "-t", coreTempTemplate host] 20 ]
                                     else [])
                              ++ (if hostFan host
                                     then [ Run $ CatInt 0 "/sys/devices/platform/thinkpad_hwmon/fan1_input" ["-L", "3170", "-H", "3900", "-l", Theme.coldest, "-h", Theme.hottest] 20 ]
                                     else [])
                              ++ commands sharedConfig
                 , template = hostTemplate host
                 }

main :: IO ()
main = getHostName >>= exportTo' "xmobarrc" . myConfig
  where exportTo' = flip exportTo

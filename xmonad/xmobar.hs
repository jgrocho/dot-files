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
primarySeparator = color Theme.foregroundSecondary "" " │ "

secondarySeparator :: T.Text
secondarySeparator = color Theme.foregroundSecondary "" " ∙ "

sharedConfig :: Config
sharedConfig =
    config { font = Theme.normalFont
           , bgColor = Theme.background
           , fgColor = Theme.foreground
           , iconRoot = "/home/jon/.xmonad/icons"
           , iconOffset = 9
           }

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
            , color Theme.cyan "" $ alias "date"
            ]
        ]

myConfig :: HostName -> Config
myConfig hostname = let host = lookupHost hostname in
    sharedConfig { position = Top
                 , commands = [ Run $ MultiCpu ["-S", "True", "-L", "3", "-H", "50", "--normal", Theme.good, "--high", Theme.bad, "-t", "<autototal>", "-p", "2"] 10
                              , Run $ Memory ["-t", "<usedipat><usedratio>%", "--", "--used-icon-pattern", "<icon=circle_filling_%%.xpm/>"] 10
                              , Run $ Date dateFormat "date" 10
                              , Run $ Network interface ["-t", "↓<rx> ↑<tx>", "-S", "False"] 10
                              , Run $ Wireless interface ["-t", "<essid> <quality>%"] 10
                              , Run $ StdinReader
                              ]
                              ++ (if hostBattery host
                                     then [ Run $ BatteryP ["BAT0"] ["-t", "<acstatus>", "--", "-O", "<leftipat><left>%" <> primarySeparator, "-o", "<leftipat><left>%" <> primarySeparator, "-i", "", "--off-icon-pattern", "<icon=battery_%%.xpm/>", "--on-icon-pattern", "<icon=battery_charging_%%.xpm/>"] 20 ]
                                     else [])
                              ++ (if hostTemp host
                                     then [ Run $ CoreTemp ["-L", "40", "-H", "60", "-l", Theme.coldest, "-h", Theme.hottest, "-t", coreTempTemplate host] 20 ]
                                     else [])
                              ++ (if hostFan host
                                     then [ Run $ CatInt 0 "/sys/devices/platform/thinkpad_hwmon/fan1_input" ["-L", "3170", "-H", "3900", "-l", Theme.coldest, "-h", Theme.hottest] 20 ]
                                     else [])
                 , template = hostTemplate host
                 }

main :: IO ()
main = do
    hostname <- getHostName
    myConfig hostname `exportTo` "xmobarrc"

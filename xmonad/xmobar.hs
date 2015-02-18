{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import XMobarHs
import Network.HostName

myConfig :: String -> Config
myConfig host =
    config { font = "xft:inconsolata:size=11"
           , bgColor = "#002b36"
           , fgColor = "#839496"
           , position = Top
           , commands = [ Run $ MultiCpu ["-L","3","-H","50","--normal","#859900","--high","#dc322f","-t","Cpu: <autototal>%"] 10
                        , Run $ Memory ["-t","Mem: <usedratio>%"] 10
                        , Run $ Date "%a %b %-d %-H:%M" "date" 10
                        , Run $ Network "wifi0" ["-t","↓<rx> ↑<tx>","-S","True"] 10
                        , Run $ Wireless "wifi0" ["-t","<essid> <quality>%"] 10
                        , Run $ StdinReader
                        ]
           , sepChar = "%"
           , alignSep = "}{"
           , template = "%StdinReader% }{ %wifi0wi% * %wifi0% | %multicpu% | %memory%    <fc=#cb4b16>%date%</fc>"
           }

main :: IO ()
main = getHostName >>= exportTo' "xmobarrc" . myConfig
  where exportTo' = flip exportTo

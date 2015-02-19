{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Monoid      ((<>))
import qualified Data.Text        as T
import           Network.HostName
import qualified Theme            as Theme
import           XMobarHs

interface :: T.Text
interface = "wifi0"

dateFormat :: T.Text
dateFormat = "%a %-d %b %-H:%M"

alias :: T.Text -> T.Text
alias = surround $ sepChar sharedConfig

primarySeparator :: T.Text
primarySeparator = xmobarColor Theme.foregroundSeconday " │ "

secondarySeparator :: T.Text
secondarySeparator = xmobarColor Theme.foregroundSeconday " ∙ "

sharedConfig :: Config
sharedConfig =
    config { font = Theme.normalFont
           , bgColor = Theme.background
           , fgColor = Theme.foreground
           , position = Top
           , commands = [ Run $ MultiCpu ["-L", "3", "-H", "50", "--normal", Theme.good, "--high", Theme.bad, "-t", "<autototal>"] 10
                        , Run $ Memory ["-t", "<usedratio>%"] 10
                        , Run $ Date dateFormat "date" 10
                        , Run $ Network interface ["-t", "↓<rx> ↑<tx>", "-S", "False"] 10
                        , Run $ Wireless interface ["-t", "<essid> <quality>%"] 10
                        , Run $ StdinReader
                        ]
           , template = T.intercalate (alignSep sharedConfig)
                [ alias "StdinReader"
                , T.intercalate primarySeparator
                    [ T.intercalate secondarySeparator $ map alias [interface <> "wi", interface]
                    , alias "multicpu"
                    , alias "memory"
                    , xmobarColor Theme.cyan $ alias "date"
                    ]
                ]
           }

myConfig :: String -> Config
myConfig host = sharedConfig

main :: IO ()
main = getHostName >>= exportTo' "xmobarrc" . myConfig
  where exportTo' = flip exportTo

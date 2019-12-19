{-# OPTIONS_GHC -Wno-missing-signatures #-}
module XMonad.My.Config where

import Data.Word (Word32)

data Config
  = Config
      { terminal           :: String
      , launcher           :: String
      , screensaver        :: String
      , hasMediaKeys       :: Bool
      , useXmobar          :: Bool
      , borderWidth        :: Word32
      , normalBorderColor  :: String
      , focusedBorderColor :: String
      }

defaultConfig :: Config
defaultConfig
  = Config
      { terminal           = "gnome-terminal"
      , launcher           = rofiLauncher
      , screensaver        = "i3lock-fancy -p"
      , hasMediaKeys       = True
      , useXmobar          = False
      , borderWidth        = 1
      , normalBorderColor  = "#5b5b5b"
      , focusedBorderColor = "#db7272"
      }

home :: Config
home
  = defaultConfig

work :: Config
work
  = defaultConfig
      { screensaver       = "xscreensaver-command -lock"
      , hasMediaKeys      = False
      , borderWidth       = 2
      , normalBorderColor = "#002b36"
      , focusedBorderColor = "#268bd2"
      }

rofiLauncher = unwords
  [ "rofi -modi drun,run -show drun"
  , "-matching fuzzy -no-levenshtein-sort -sort"
  , "-theme lb -show-icons -kb-mode-next Alt+m"
  ]

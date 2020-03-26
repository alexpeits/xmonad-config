{-# OPTIONS_GHC -Wno-missing-signatures #-}
module XMonad.My.Config where

import Data.Word (Word32)

data WindowView
  = View
  | GreedyView

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
      , windowView         :: WindowView
      }

defaultConfig :: Config
defaultConfig
  = Config
      { terminal           = "gnome-terminal"
      , launcher           = rofiLauncher
      , screensaver        = "i3lock-fancy -p"
      , hasMediaKeys       = True
      , useXmobar          = False
      , borderWidth        = 2
      , normalBorderColor  = "#27444c"
      , focusedBorderColor = "#268bd2"
      , windowView         = View
      }

home :: Config
home
  = defaultConfig
      { windowView = GreedyView
      }

work :: Config
work
  = defaultConfig
      { screensaver       = "xscreensaver-command -lock"
      , hasMediaKeys      = False
      , borderWidth       = 2
      , normalBorderColor = "#27444c"
      , focusedBorderColor = "#268bd2"
      }

rofiLauncher = unwords
  [ "rofi -modi drun,run -show drun"
  , "-matching fuzzy -no-levenshtein-sort -sort"
  , "-theme lb -show-icons -kb-mode-next Alt+m"
  ]

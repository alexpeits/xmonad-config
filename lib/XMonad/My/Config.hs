{-# OPTIONS_GHC -Wno-missing-signatures #-}
module XMonad.My.Config where

data Config
  = Config
      { terminal     :: String
      , launcher     :: String
      , screensaver  :: String
      , hasMediaKeys :: Bool
      }

defaultConfig :: Config
defaultConfig
  = Config
      { terminal     = "gnome-terminal"
      , launcher     = rofiLauncher
      , screensaver  = "i3lock-fancy -p"
      , hasMediaKeys = True
      }

home :: Config
home
  = defaultConfig

work :: Config
work
  = defaultConfig
      { screensaver  = "xscreensaver-command -lock"
      , hasMediaKeys = False
      }

rofiLauncher = unwords
  [ "rofi -modi drun,run -show drun"
  , "-matching fuzzy -no-levenshtein-sort -sort"
  , "-theme lb -show-icons -kb-mode-next Alt+m"
  ]

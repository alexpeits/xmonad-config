module XMonad.My.Config where

import Data.Word        (Word32)
import Network.HostName (HostName)

configs :: [(HostName, Config)]
configs
  = [ ("nixos", home)
    , ("trilobite", work)
    ]

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
      , tabColor           :: String
      , topBarHeight       :: Word32
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
      , normalBorderColor  = "#234540"
      , focusedBorderColor = "#259488"
      , tabColor           = "#246156"
      , topBarHeight       = 7
      , windowView         = GreedyView
      }

home :: Config
home
  = defaultConfig

work :: Config
work
  = defaultConfig
      { screensaver  = "xscreensaver-command -lock"
      , hasMediaKeys = False
      , topBarHeight = 10
      , windowView   = View
      }

rofiLauncher = unwords
  [ "rofi -modi drun,run -show drun"
  , "-matching fuzzy -no-levenshtein-sort -sort"
  , "-theme lb -show-icons -kb-mode-next Alt+m"
  ]

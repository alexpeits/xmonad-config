module XMonad.My.Config where

import Data.Word        (Word32)
import Network.HostName (HostName)

configs :: [(HostName, Config)]
configs
  = [ ("seabeast", home)
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
      , xmobarConf         :: String
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
      , screensaver        = "i3lock-wrap"
      , hasMediaKeys       = True
      , xmobarConf         = "~/.xmonad/xmobar.hs"
      , borderWidth        = 2
      , normalBorderColor  = "#27444c"
      , focusedBorderColor = "#268bd2"
      , tabColor           = "#245361"
      , topBarHeight       = 7
      , windowView         = GreedyView
      }

home :: Config
home
  = defaultConfig
      { xmobarConf = "~/.xmonad/xmobar.hs"
      }

work :: Config
work
  = defaultConfig
      { screensaver  = "xscreensaver-command -lock"
      , hasMediaKeys = False
      , xmobarConf   = "~/.xmonad/xmobar_nonlaptop.hs"
      , topBarHeight = 10
      , windowView   = View
      }

rofiLauncher = unwords
  [ "rofi -modi drun,run -show drun"
  , "-matching fuzzy -sorting-method fzf -sort"
  , "-theme lb -show-icons -kb-mode-next Alt+m"
  ]

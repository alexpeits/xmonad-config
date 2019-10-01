module XMonad.My.Config where

import Data.Ratio (Rational)


data Config
  = Config
      { screens      :: ScreenConfig
      , panelHeight  :: Int
      , panelScreen  :: Resolution
      , terminal     :: String
      , launcher     :: String
      , screensaver  :: String
      , hasMediaKeys :: Bool
      }

data ScreenConfig
  = SingleScreen Resolution
  | DualScreenHorizontal Resolution Resolution

data Resolution
  = Resolution
      { w :: Rational
      , h :: Rational
      }

singleScreen :: Config
singleScreen
  = Config
      { screens      = SingleScreen res
      , panelHeight  = 25
      , panelScreen  = res
      , terminal     = "gnome-terminal"
      , launcher     = rofiLauncher
      , screensaver  = "i3lock-fancy -p"
      , hasMediaKeys = True
      }
  where
    res = Resolution 1920 1080

dualScreenHorizontal :: Config
dualScreenHorizontal
  = Config
      { screens      = DualScreenHorizontal right left
      , panelHeight  = 28
      , panelScreen  = right
      , terminal     = "gnome-terminal"
      , launcher     = rofiLauncher
      , screensaver  = "xscreensaver-command -lock"
      , hasMediaKeys = False
      }
  where
    -- TODO
    right = Resolution 1 1
    left = Resolution 1 1

rofiLauncher = unwords
  [ "rofi -modi drun,run -show drun"
  , "-matching fuzzy -no-levenshtein-sort -sort"
  , "-theme lb -show-icons -kb-mode-next Alt+m"
  ]

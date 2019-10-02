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

defaultConfig :: Config
defaultConfig
  = Config
      { screens      = defaultScreenConfig
      , panelHeight  = 25
      , panelScreen  = defaultResolution
      , terminal     = "gnome-terminal"
      , launcher     = rofiLauncher
      , screensaver  = "i3lock-fancy -p"
      , hasMediaKeys = True
      }

data ScreenConfig
  = SingleScreen Resolution
  | DualScreenHorizontal Resolution Resolution

defaultScreenConfig :: ScreenConfig
defaultScreenConfig
  = SingleScreen defaultResolution

data Resolution
  = Resolution
      { w :: Rational
      , h :: Rational
      }

defaultResolution :: Resolution
defaultResolution
  = Resolution
      { w = 1920
      , h = 1080
      }


thinkpad13 :: Config
thinkpad13
  = defaultConfig

workHorizontal :: Config
workHorizontal
  = defaultConfig
      { screens      = DualScreenHorizontal right left
      , panelHeight  = 28
      , panelScreen  = right
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

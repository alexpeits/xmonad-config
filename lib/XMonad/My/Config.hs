{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
module XMonad.My.Config where

import Data.Word (Word32)

import qualified Data.Aeson as Ae
import Data.Aeson ((.:?), (.!=))

data WindowView
  = View
  | GreedyView

instance Ae.FromJSON WindowView where
  parseJSON = Ae.withText "WindowView" $ \case
    "View" -> pure View
    "GreedyView" -> pure GreedyView
    v -> fail $ "Invalid WindowView value: " <> show v

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

instance Ae.FromJSON Config where
  parseJSON = Ae.withObject "Config" $ \o ->
    Config
      <$> o .:? "terminal" .!= terminal defaultConfig
      <*> o .:? "launcher" .!= launcher defaultConfig
      <*> o .:? "screensaver" .!= screensaver defaultConfig
      <*> o .:? "hasMediaKeys" .!= hasMediaKeys defaultConfig
      <*> o .:? "useXmobar" .!= useXmobar defaultConfig
      <*> o .:? "borderWidth" .!= borderWidth defaultConfig
      <*> o .:? "normalBorderColor" .!= normalBorderColor defaultConfig
      <*> o .:? "focusedBorderColor" .!= focusedBorderColor defaultConfig
      <*> o .:? "windowView" .!= windowView defaultConfig

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
      , windowView         = GreedyView
      }

home :: Config
home
  = defaultConfig

work :: Config
work
  = defaultConfig
      { screensaver        = "xscreensaver-command -lock"
      , hasMediaKeys       = False
      , borderWidth        = 2
      , normalBorderColor  = "#27444c"
      , focusedBorderColor = "#268bd2"
      , windowView         = View
      }

rofiLauncher = unwords
  [ "rofi -modi drun,run -show drun"
  , "-matching fuzzy -no-levenshtein-sort -sort"
  , "-theme lb -show-icons -kb-mode-next Alt+m"
  ]

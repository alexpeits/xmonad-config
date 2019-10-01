{-# LANGUAGE RecordWildCards #-}
module XMonad.My.Windows where

import           Data.Ratio                  (Rational)

import           XMonad                      ( composeAll, className
                                             , doShift, doIgnore, (=?)
                                             , (-->)
                                             )

import           XMonad.Hooks.ManageHelpers  (doFloatAt, doFullFloat
                                             , isFullscreen
                                             )
import qualified XMonad.StackSet             as W
import           XMonad.Util.NamedScratchpad (customFloating)

import qualified XMonad.My.Config            as Cfg


nWorkspace :: [String] -> Int -> String
nWorkspace ws i = ws !! (i - 1)

dualScreen _
  = composeAll
      [ className =? "Gnome-calculator" --> smallRectTR
      , className =? "Indicator.py" --> doFloatAt 0.43 0.43
      , className =? "Zenity" --> doFloatAt 0.43 0.43
      , className =? "Gsimplecal" --> doFloatAt 0.815 0.022
      , className =? "stalonetray" --> doIgnore
      , isFullscreen --> doFullFloat
      ]

singleScreen wsp
  = composeAll
      [ className =? "Emacs" --> doShift (nWorkspace wsp 3)
      , className =? "Slack" --> doShift (nWorkspace wsp 4)
      , className =? "Skype" --> doShift (nWorkspace wsp 4)
      , className =? "Pidgin" --> doShift (nWorkspace wsp 4)
      , className =? "vlc" --> doShift (nWorkspace wsp 5)
      , className =? "Spotify" --> doShift (nWorkspace wsp 5)
      , className =? "spotify" --> doShift (nWorkspace wsp 5)
      , className =? "VirtualBox" --> doShift (nWorkspace wsp 6)
      , className =? "VirtualBox Manager" --> doShift (nWorkspace wsp 6)
      , dualScreen wsp
      ]

-- TODO
topMargin :: Rational -> Cfg.Resolution -> Rational
topMargin panelHeight Cfg.Resolution{..}
  = panelHeight / h

myTopMargin =
  28 / 2160

middleRR w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h
topRightRR w h = W.RationalRect (1 - w) myTopMargin w h
topLeftRR w h = W.RationalRect 0 myTopMargin w h
botRightRR w h = W.RationalRect (1 - w) (1 - h) w h
botLeftRR w h = W.RationalRect 0 (1 - h) w h
dropDownRR w h = W.RationalRect 0 myTopMargin w h

largeRectM = customFloating $ middleRR 0.8 0.8
medRectM = customFloating $ middleRR 0.65 0.75
medRectBR = customFloating $ botRightRR 0.45 0.5
smallRectTR = customFloating $ topRightRR 0.25 0.3
smallRectBR = customFloating $ botRightRR 0.3 0.4
dropDown = customFloating $ dropDownRR 1 0.35


getWindows Cfg.Config{..}
  = case screens of
      Cfg.SingleScreen _ -> singleScreen
      Cfg.DualScreenHorizontal _ _ -> dualScreen

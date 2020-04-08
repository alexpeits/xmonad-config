module XMonad.My.Scratchpad where

import           XMonad (X, className, title, stringProperty, (=?))

import qualified XMonad.Util.NamedScratchpad as NS

import qualified XMonad.My.Windows as W


getScratch :: String -> X ()
getScratch
  = NS.namedScratchpadAction scratchpads

scratchpads :: [NS.NamedScratchpad]
scratchpads =
  [ NS.NS
      "firefox"
      "firefox --new-instance --class firefox-sp -P Simple"
      (className =? "firefox-sp")
      W.medRectBR
  , NS.NS
      "emacs"
      ( unwords
          [ "emacs --name emacs-sp"
          , "--eval '(progn"
          , "(persp-switch \"elfeed\")"
          , "(elfeed)"
          , "(persp-switch \"config\")"
          , "(find-file \"~/.emacs.d/configuration.org\")"
          , "(persp-switch \"main\")"
          , "(bookmark-jump \"shared-notes\")"
          , "(persp-rename \"notes\")"
          , ")'"
          ]
      )
      (title =? "emacs-sp")
      W.medRectM
  , NS.NS
      "terminal-dropdown"
      "gnome-terminal --role=terminal-dropdown-sp"
      (role =? "terminal-dropdown-sp")
      W.dropDown
  , NS.NS
      "terminal"
      "gnome-terminal --role=terminal-sp"
      (role =? "terminal-sp")
      W.smallRectM
  , NS.NS
      "nautilus"
      "nautilus --new-window --class=nautilus-sp"
      (className =? "nautilus-sp")
      W.medRectM
  ]
  where
    role  = stringProperty "WM_WINDOW_ROLE"


module XMonad.My.Scratchpad where

import XMonad (X, className, stringProperty, title, (=?))
import qualified XMonad.My.Windows as W
import qualified XMonad.Util.NamedScratchpad as NS

getScratch :: String -> X ()
getScratch =
  NS.namedScratchpadAction scratchpads

scratchpads :: [NS.NamedScratchpad]
scratchpads = map snd scratchpads'

-- | Names that will show up on xmobar (setup with `ppExtras`)
scratchpadNames :: [String]
scratchpadNames = map fst scratchpads'

scratchpads' :: [(String, NS.NamedScratchpad)]
scratchpads' =
  [ ( "f",
      NS.NS
        "firefox"
        "firefox --new-instance --class firefox-sp"
        (className =? "firefox-sp")
        W.medRectBR
    ),
    ( "e",
      NS.NS
        "emacs"
        ( unwords
            [ "emacs --name emacs-sp",
              "--eval '(progn",
              "(persp-switch \"elfeed\")",
              "(elfeed)",
              "(persp-switch \"config\")",
              "(find-file \"~/.emacs.d/configuration.org\")",
              "(persp-switch \"main\")",
              "(find-file (expand-file-name \"home.org\" my/org-directory))",
              "(outline-show-all)",
              "(persp-rename \"roam\")",
              ")'"
            ]
        )
        (title =? "emacs-sp")
        W.largeRectM
    ),
    ( "",
      NS.NS
        "terminal-dropdown"
        "gnome-terminal --role=terminal-dropdown-sp"
        (role =? "terminal-dropdown-sp")
        W.dropDown
    ),
    ( "",
      NS.NS
        "terminal"
        "gnome-terminal --role=terminal-sp"
        (role =? "terminal-sp")
        W.smallRectM
    ),
    ( "",
      NS.NS
        "nautilus"
        "nautilus --new-window --class=nautilus-sp"
        (className =? "nautilus-sp")
        W.medRectM
    ),
    ( "",
      NS.NS
        "bitwarden"
        "bitwarden"
        (className =? "Bitwarden")
        W.medRectM
    ),
    ( "",
      NS.NS
        "pavucontrol"
        "pavucontrol --class=pavucontrol-sp"
        (className =? "pavucontrol-sp")
        W.medRectM
    )
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

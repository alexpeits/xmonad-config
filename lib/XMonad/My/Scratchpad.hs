module XMonad.My.Scratchpad where

import           XMonad (X, className, title, stringProperty, (=?))

import qualified XMonad.Util.NamedScratchpad as NS

import qualified XMonad.My.Windows as W


getScratch :: String -> X ()
getScratch
  = NS.namedScratchpadAction scratchpads

scratchpads :: [NS.NamedScratchpad]
scratchpads = map snd scratchpads'

-- | Names that will show up on xmobar (setup with `ppExtras`)
scratchpadNames :: [String]
scratchpadNames = map fst scratchpads'

scratchpads' :: [(String, NS.NamedScratchpad)]
scratchpads' =
  [ ( "φ"
    , NS.NS
        "firefox"
        "firefox --new-instance --class firefox-sp"
        (className =? "firefox-sp")
        W.medRectBR
    )
  , ( "ε"
    , NS.NS
        "emacs"
        ( unwords
            [ "emacs --name emacs-sp"
            , "--eval '(progn"
            , "(persp-switch \"elfeed\")"
            , "(elfeed)"
            , "(persp-switch \"config\")"
            , "(find-file \"~/.emacs.d/configuration.org\")"
            , "(persp-switch \"notes\")"
            , "(bookmark-jump \"shared-notes\")"
            , "(persp-switch \"main\")"
            , "(deft)"
            , "(persp-rename \"roam\")"
            , "(unless (server-running-p) (server-mode))"
            , ")'"
            ]
        )
        (title =? "emacs-sp")
        W.medRectM
    )
  , ( ""
    , NS.NS
        "terminal-dropdown"
        "gnome-terminal --role=terminal-dropdown-sp"
        (role =? "terminal-dropdown-sp")
        W.dropDown
    )
  , ( ""
    , NS.NS
        "terminal"
        "gnome-terminal --role=terminal-sp"
        (role =? "terminal-sp")
        W.smallRectM
    )
  , ( ""
    , NS.NS
       "nautilus"
       "nautilus --new-window --class=nautilus-sp"
       (className =? "nautilus-sp")
       W.medRectM
    )
  ]
  where
    role  = stringProperty "WM_WINDOW_ROLE"


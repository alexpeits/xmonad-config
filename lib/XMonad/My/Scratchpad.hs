{-# OPTIONS_GHC -Wno-missing-signatures #-}
module XMonad.My.Scratchpad where

import           XMonad                      (className, (=?), stringProperty)

import qualified XMonad.Util.NamedScratchpad as NS

import           XMonad.My.Windows


scratchpads =
  [ NS.NS
      "scratch"
      "gedit --class=Scratch ~/.scratch.txt"
      (className =? "Scratch")
      smallRectBR
  , NS.NS
      "docs"
      firefoxDocs
      (className =? "Docs")
      medRectBR
  , NS.NS
      "centerChrome"
      chrome
      (className =? "centerChrome")
      medRectM
  , NS.NS
      "dropTerm"
      "gnome-terminal --role=dropTerm"
      (role =? "dropTerm")
      dropDown
  , NS.NS
      "centerTerm"
      "gnome-terminal --role=centerTerm"
      (role =? "centerTerm")
      smallRectM
  , NS.NS
      "files"
      "nautilus --new-window --class=nautilusScratch"
      (className =? "nautilusScratch")
      medRectM
  ]
  where role  = stringProperty "WM_WINDOW_ROLE"
        -- title = stringProperty "WM_NAME"

actions = NS.namedScratchpadAction scratchpads

firefoxDocs = unwords
  [ "firefox"
  , "--new-instance --class Docs -P Simple"
  , "https://hoogle.haskell.org"
  , "https://www.haskell.org/hoogle"
  , "https://pursuit.purescript.org"
  ]

chrome = unwords
  [ "google-chrome-stable"
  , "--new-window"
  , "--user-data-dir=/tmp/tmp-chrome"
  , "--class=centerChrome"
  ]

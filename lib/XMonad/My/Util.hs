module XMonad.My.Util where

import XMonad
import XMonad.Util.Run    (spawnPipe, safeSpawn, runProcessWithInput)
import XMonad.Util.Ungrab (unGrab)


selectScreenshot = "gnome-screenshot -a"
windowScreenshot = "scrot ~/Pictures/Screenshots/%Y-%m-%d_%H-%M-%S.png -s"
screenshot = "gnome-screenshot"

getScreenshot :: X ()
getScreenshot = do
  scrStr <-
    runProcessWithInput
      "rofi-dmenu-args.sh"
      ["Screenshot type", "Area", "Window", "Full"]
      ""
  case filter (/= '\n') scrStr of
    "Area"   -> spawn selectScreenshot
    "Window" -> unGrab >> spawn windowScreenshot
    "Full"   -> spawn screenshot
    _        -> return ()

module XMonad.My.Util where

import Control.Monad (when)
import XMonad
import qualified XMonad.Actions.GridSelect as GS
import XMonad.Util.Ungrab (unGrab)

spawnSelectedWithLabel ::
  GS.GSConfig (String, Bool) ->
  [(String, (String, Bool))] ->
  X ()
spawnSelectedWithLabel conf pairs =
  GS.gridselect conf pairs >>= flip whenJust doSpawn
  where
    doSpawn :: (String, Bool) -> X ()
    doSpawn (exe, shouldUnGrab) = do
      when shouldUnGrab unGrab
      spawn exe

scrot :: String -> String
scrot =
  ("scrot ~/Pictures/Screenshots/%Y-%m-%d_%H-%M-%S.png " <>)

getScreenshot :: X ()
getScreenshot =
  spawnSelectedWithLabel
    def
    [ ("area or window", (selectAreaOrWindow, True)),
      ("window", (curWindow, False)),
      ("fullscreen", (fullScreen, False))
    ]
  where
    selectAreaOrWindow = scrot "-s"
    curWindow = scrot "-u"
    fullScreen = scrot ""

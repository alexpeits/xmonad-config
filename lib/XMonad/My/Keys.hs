{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module XMonad.My.Keys where

import qualified Data.Map                         as M
import qualified System.Exit                      as Sys

import qualified XMonad                           as X
import           XMonad                           hiding (workspaces, terminal, keys)
import           XMonad.Operations                (windows)

import qualified XMonad.Actions.CopyWindow        as CopyWindow
import qualified XMonad.Actions.CycleWS           as Cycle
import qualified XMonad.Actions.GridSelect        as GS
import           XMonad.Actions.WindowBringer     (gotoMenuArgs', bringMenuArgs')
import           XMonad.Hooks.ManageDocks         (ToggleStruts(..))
import qualified XMonad.Layout.Maximize           as Maximize
import qualified XMonad.Layout.MultiToggle        as MultiToggle
import qualified XMonad.Layout.Reflect            as Reflect
import qualified XMonad.Layout.ResizableTile      as ResizableTile
import qualified XMonad.Layout.ToggleLayouts      as ToggleLayouts
import qualified XMonad.Layout.WindowNavigation   as WNav
import qualified XMonad.StackSet                  as W

import qualified XMonad.My.Config                 as Cfg
import qualified XMonad.My.Scratchpad             as Scratch
import qualified XMonad.My.Util                   as Util
import qualified XMonad.My.Windows                as Windows

import           Graphics.X11.ExtraTypes.XF86


-- | Copy current window to all workspaces or delete all other copies
copyToAllOrKillOther :: X ()
copyToAllOrKillOther = do
  inOtherWs <- CopyWindow.wsContainingCopies
  if null inOtherWs
    then windows CopyWindow.copyToAll
    else CopyWindow.killAllOtherCopies

customKeys cfg@Cfg.Config{..} conf@XConfig{modMask = modMask} =
  -- terminal
  [ ((modMask .|. shiftMask, xK_Return), spawn $ X.terminal conf)
  , ((modMask .|. controlMask .|. shiftMask, xK_Return), spawn "TERM_LIGHT=1 gnome-terminal --profile=Light")

  -- screen lock
  , ((modMask .|. controlMask, xK_l), spawn screensaver)

  -- maximize current window
  , ((modMask, xK_backslash), withFocused (sendMessage . Maximize.maximizeRestore))

  -- copy current win to all ws or kill other copies
  -- NOTE: doesn't really work with multiple monitors
  , ((modMask, xK_r), copyToAllOrKillOther)

  -- float a window and cycle its position to the screen corners
  , ((modMask, xK_f), withFocused Windows.cycleCorner)

  -- launcher
  , ((modMask, xK_p), spawn launcher)
  -- window select & bring
  , ((modMask .|. shiftMask, xK_p) , gotoMenuArgs' "rofi" (rofiDmenuArgs "Go to window"))
  , ((modMask .|. controlMask .|. shiftMask, xK_p), bringMenuArgs' "rofi" (rofiDmenuArgs "Bring window"))
  -- screenshots
  , ((modMask .|. shiftMask, xK_s), Util.getScreenshot)
  , ((modMask, xK_Print), Util.getScreenshot)

  -- scratchpads
  , ((0, xK_F12), Scratch.getScratch "terminal-dropdown")
  , ((modMask, xK_F12), Scratch.getScratch "terminal")
  , ((modMask, xK_F8), Scratch.getScratch "emacs")
  , ((modMask .|. shiftMask, xK_d), Scratch.getScratch "firefox")
  , ((modMask .|. shiftMask, xK_b), Scratch.getScratch "nautilus")

  -- toggle xmobar
  , ((modMask .|. shiftMask, xK_f), sendMessage ToggleStruts)

  -- brightness up
  , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set 5+")
  -- brightness down
  , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 5-")

  -- reflect horizontally
  , ((modMask , xK_m), sendMessage $ MultiToggle.Toggle Reflect.REFLECTX)
  -- reflect vertically
  , ((modMask .|. shiftMask, xK_m), sendMessage $ MultiToggle.Toggle Reflect.REFLECTY)
  -- toggle layouts, used to remove top bar
  , ((modMask, xK_d), sendMessage ToggleLayouts.ToggleLayout)

  -- expand vertically
  , ((modMask .|. shiftMask , xK_h), sendMessage ResizableTile.MirrorExpand)
  -- shrink vertically
  , ((modMask .|. shiftMask , xK_l), sendMessage ResizableTile.MirrorShrink)

  -- cycle monitors
  , ((modMask, xK_o), Cycle.nextScreen)
  , ((modMask .|. shiftMask, xK_o), Cycle.shiftNextScreen)

  -- , ((modMask .|. shiftMask, xK_q), spawn "xfce4-session-logout")
  -- , ((modMask .|. shiftMask, xK_q), io (Sys.exitWith Sys.ExitSuccess))
  , ((modMask .|. shiftMask, xK_q), spawn "~/bin/session-quit")

  , ((modMask .|. controlMask .|. shiftMask, xK_j), windows W.swapDown >> windows W.focusUp)
  , ((modMask .|. controlMask .|. shiftMask, xK_k), windows W.swapUp >> windows W.focusDown)

  -- media keys
  , ((0, xF86XK_AudioMute), spawn $ volumeToggle +++ volumeShow)
  , ((0, xF86XK_AudioLowerVolume), spawn $ volumeUnmute +++ volumeDown +++ volumeShow)
  , ((0, xF86XK_AudioRaiseVolume), spawn $ volumeUnmute +++ volumeUp +++ volumeShow)
  , ((0, xF86XK_AudioPrev), spawn playerPrevious)
  , ((0, xF86XK_AudioNext), spawn playerNext)
  , ((0, xF86XK_AudioPlay), spawn playerPlayPause)
  , ((0, xF86XK_AudioStop), spawn playerStop)

  -- temporary
  , ((modMask, xK_equal), spawn "PATH=~/bin:$PATH ~/bin/contrall.sh")
  , ((mod1Mask, xK_F11), spawn "~/bin/screen-layout.sh --auto")
  , ((mod1Mask, xK_F12), spawn "systemctl --user restart keyboard")
  ]

  ++

  let
    view
      = case Cfg.windowView cfg of
          Cfg.View -> W.view
          Cfg.GreedyView -> W.greedyView
  in
    [ ( (m .|. modMask, k), windows (f i) )
    | (i, k) <- zip (X.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [ (view, 0) , (W.shift, shiftMask) ]
    ]

  ++

  -- TODO
  if hasMediaKeys

    then [
           ((modMask .|. controlMask, xK_F2), spawn volumeDown)
         , ((modMask .|. controlMask, xK_F4), spawn volumeUp)
         , ((0, xF86XK_AudioMicMute), spawn "amixer -q set Capture toggle")
         , ((modMask, xK_F2), spawn playerPrevious)
         , ((modMask, xK_F3), spawn playerPlayPause)
         , ((modMask, xK_F4), spawn playerNext)
         ]

    else [ ((modMask, xK_F2), spawn "amixer -D pulse set Master 3%-")
         , ((modMask, xK_F3), spawn "amixer -D pulse set Master 1+ toggle")
         , ((modMask, xK_F4), spawn "amixer -D pulse set Master 3%+")
         , ((modMask .|. controlMask, xK_F2), spawn playerPrevious)
         , ((modMask .|. controlMask, xK_F3), spawn playerPlayPause)
         , ((modMask .|. controlMask, xK_F4), spawn playerNext)
         ]

getKeys cfg x = M.union (M.fromList (customKeys cfg x)) (X.keys def x)

volumeToggle = "amixer set Master toggle"
volumeUnmute = "amixer set Master on"
volumeDown = "amixer -q set Master 3%- && ~/bin/show-volume.sh"
volumeUp = "amixer -q set Master 3%+ && ~/bin/show-volume.sh"
volumeShow = "~/bin/show-volume.sh"

playerctl = "playerctl -p spotify,vlc "
playerPlayPause = playerctl <> "play-pause"
playerStop = playerctl <> "stop"
playerPrevious = playerctl <> "previous"
playerNext = playerctl <> "next"

(+++) :: String -> String -> String
x +++ y = x ++ " && " ++ y
infixr 5 +++

rofiDmenuArgs prompt =
  [ "-dmenu"
  , "-i", "-p", prompt
  , "-matching", "fuzzy", "-sorting-method", "fzf", "-sort"
  , "-theme", "lb"
  ]

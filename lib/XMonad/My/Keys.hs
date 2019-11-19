{-# LANGUAGE RecordWildCards #-}
module XMonad.My.Keys where

import qualified Data.Map                         as M

import qualified XMonad                           as X
import           XMonad                           hiding (workspaces, terminal, keys)
import           XMonad.Operations                (windows)

import qualified XMonad.Actions.CopyWindow        as CopyWindow
import qualified XMonad.Actions.CycleWS           as Cycle
import qualified XMonad.Actions.GridSelect        as GS
import           XMonad.Actions.WindowBringer     (gotoMenuArgs')
import           XMonad.Hooks.ManageDocks         (ToggleStruts(..))
import qualified XMonad.Layout.IndependentScreens as IndS
import qualified XMonad.Layout.Maximize           as Maximize
import qualified XMonad.Layout.MultiToggle        as MultiToggle
import qualified XMonad.Layout.Reflect            as Reflect
import qualified XMonad.Layout.WindowNavigation   as WNav
import qualified XMonad.StackSet                  as W

import qualified XMonad.My.Config                 as Cfg
import qualified XMonad.My.Scratchpad             as Scratch
import qualified XMonad.My.Util                   as Util

import           Graphics.X11.ExtraTypes.XF86


-- | Copy current window to all workspaces or delete all other copies
copyToAllOrKillOther :: X ()
copyToAllOrKillOther = do
  inOtherWs <- CopyWindow.wsContainingCopies
  if null inOtherWs
    then windows CopyWindow.copyToAll
    else CopyWindow.killAllOtherCopies

customKeys Cfg.Config{..} conf@XConfig{modMask = modMask} =
  -- terminal
  [ ((modMask .|. shiftMask, xK_Return), spawn $ X.terminal conf)
  , ((modMask .|. controlMask .|. shiftMask, xK_Return), spawn "gnome-terminal --profile=Light")

  -- screen lock
  , ((modMask .|. controlMask, xK_l), spawn screensaver)

  -- maximize current window
  , ((modMask, xK_backslash), withFocused (sendMessage . Maximize.maximizeRestore))

  -- copy current win to all ws or kill other copies
  , ((modMask, xK_r), copyToAllOrKillOther)

  -- launcher
  , ((modMask, xK_p), spawn launcher)
  -- window select
  , ((modMask .|. shiftMask, xK_p) , gotoMenuArgs' "rofi" rofiGoToWinArgs)
  -- grid select
  , ((modMask .|. shiftMask, xK_g), GS.goToSelected def)
  -- screenshots
  , ((modMask .|. controlMask .|. shiftMask, xK_p), Util.getScreenshot)

  -- scratchpads
  , ((modMask, xK_z), Scratch.actions "dropTerm")
  , ((0, xK_F12), Scratch.actions "dropTerm")
  , ((modMask .|. shiftMask, xK_n), Scratch.actions "scratch")
  , ((modMask .|. shiftMask, xK_d), Scratch.actions "docs")
  , ((modMask .|. shiftMask, xK_b), Scratch.actions "files")

  -- toggle xmobar
  , ((modMask .|. shiftMask, xK_f), sendMessage ToggleStruts)

  -- Toggle CM keyboard led on/off
  , ((0, xK_Scroll_Lock), spawn "~/bin/cmstorm_led")

  -- brightness up
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5%")
  -- brightness down
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5%")

  -- move windows to sublayouts
  , ((modMask .|. shiftMask, xK_Left), sendMessage $ WNav.Move WNav.L)
  , ((modMask .|. shiftMask, xK_Right), sendMessage $ WNav.Move WNav.R)
  , ((modMask .|. shiftMask, xK_Up), sendMessage $ WNav.Move WNav.U)
  , ((modMask .|. shiftMask, xK_Down), sendMessage $ WNav.Move WNav.D)

  -- reflect horizontally
  , ((modMask .|. shiftMask, xK_h), sendMessage $ MultiToggle.Toggle Reflect.REFLECTX)
  -- reflect vertically
  , ((modMask .|. shiftMask, xK_v), sendMessage $ MultiToggle.Toggle Reflect.REFLECTY)

  -- cycle monitors
  , ((modMask, xK_o), Cycle.nextScreen)
  , ((modMask .|. shiftMask, xK_o), Cycle.shiftNextScreen)

  , ((modMask .|. shiftMask, xK_q), spawn "xfce4-session-logout")

  , ((modMask .|. controlMask .|. shiftMask, xK_j), windows W.swapDown >> windows W.focusUp)
  , ((modMask .|. controlMask .|. shiftMask, xK_k), windows W.swapUp >> windows W.focusDown)

  -- media keys
  , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%-")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")
  , ((0, xF86XK_AudioPrev), spawn "playerctl previous")
  , ((0, xF86XK_AudioNext), spawn "playerctl next")
  , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
  , ((0, xF86XK_AudioStop), spawn "playerctl stop")

  ]

  ++

  case screens of

    Cfg.SingleScreen _
      -> [((m .|. modMask, k), windows $  f i)
         | (i, k) <- zip (X.workspaces conf) [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    _
      -> [((m .|. modMask, k), windows $ IndS.onCurrentScreen f i)
         | (i, k) <- zip (IndS.workspaces' conf) [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

  ++

  if hasMediaKeys

    -- then [ ((0, xF86XK_AudioMute), spawn "amixer -D pulse set Master 1+ toggle")
    then [
         -- , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")
         -- , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")
         -- laptop + bluetooth = disaster
           ((modMask .|. controlMask, xK_F2), spawn "amixer -q set Master 5%-")
         , ((modMask .|. controlMask, xK_F4), spawn "amixer -q set Master 5%+")
         , ((0, xF86XK_AudioMicMute), spawn "amixer -q set Capture toggle")
         , ((modMask, xK_F2), spawn "playerctl previous")
         , ((modMask, xK_F3), spawn "playerctl play-pause")
         , ((modMask, xK_F4), spawn "playerctl next")

         , ((modMask, xK_F5), spawn "xbacklight -dec 2")
         , ((modMask, xK_F6), spawn "xbacklight -inc 2")
         ]

    else [ ((modMask, xK_F2), spawn "amixer -D pulse set Master 5%-")
         , ((modMask, xK_F3), spawn "amixer -D pulse set Master 1+ toggle")
         , ((modMask, xK_F4), spawn "amixer -D pulse set Master 5%+")
         , ((modMask .|. controlMask, xK_F2), spawn "playerctl previous")
         , ((modMask .|. controlMask, xK_F3), spawn "playerctl play-pause")
         , ((modMask .|. controlMask, xK_F4), spawn "playerctl next")
         ]

getKeys cfg x = M.union (M.fromList (customKeys cfg x)) (X.keys def x)


rofiGoToWinArgs =
  [ "-dmenu"
  , "-i", "-p", "Go to window"
  , "-matching", "fuzzy", "-no-levenshtein-sort", "-sort"
  , "-theme", "lb"
  ]

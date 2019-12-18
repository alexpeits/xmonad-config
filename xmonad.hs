{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE CPP #-}
import           Control.Monad               (when)
import           System.IO                   (hPutStrLn)

import           XMonad

import           XMonad.Config.Xfce          (xfceConfig)

import qualified XMonad.Hooks.DynamicLog     as DL
import           XMonad.Hooks.EwmhDesktops   (ewmh)
import           XMonad.Hooks.InsertPosition (insertPosition, Position(..), Focus(..))
import           XMonad.Hooks.ManageDocks    (manageDocks, docksEventHook)
import           XMonad.Hooks.ManageHelpers  (composeOne, isDialog, (-?>), doCenterFloat, transience)

import           XMonad.Layout.Fullscreen    (fullscreenSupport)
import           XMonad.Layout.NoBorders     (smartBorders)

import           XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
import           XMonad.Util.Run             (spawnPipe)

import qualified XMonad.My.Config            as My.Cfg
import qualified XMonad.My.Keys              as My.Keys
import qualified XMonad.My.Layouts           as My.Layouts
import qualified XMonad.My.Scratchpad        as My.Scratchpad
import qualified XMonad.My.Windows           as My.Windows
import qualified XMonad.My.Workspaces        as My.Workspaces

xmobarPanel process = DL.dynamicLogWithPP $ DL.xmobarPP
  { DL.ppOutput  = hPutStrLn process
  , DL.ppTitle   = DL.xmobarColor "#d58966" "" . DL.shorten 100
  , DL.ppCurrent = DL.xmobarColor "#2ec8a2" "" . DL.wrap "[" "]"
  , DL.ppVisible = DL.xmobarColor "#3b7887" "" . DL.wrap "(" ")"
  , DL.ppHidden  = \ws -> if ws == "NSP" then "" else ws
  , DL.ppSep     = DL.xmobarColor "#676767" "" " | "
  , DL.ppLayout  = DL.xmobarColor "#676767" ""
  , DL.ppUrgent  = DL.xmobarColor "red" "yellow"
  }

main = do
  let
#ifdef WORK
    cfg = My.Cfg.work
    layout = My.Layouts.topBar
#else
    cfg = My.Cfg.home
    layout = My.Layouts.noTopBar
#endif
    wsp = My.Workspaces.workspaces
    -- myLogHook =
      -- when (My.Cfg.useXmobar cfg) $
        -- spawnPipe "xmobar ~/.xmonad/xmobar.hs" >>= xmobarPanel
  xmonad $ fullscreenSupport $ ewmh $ xfceConfig
    { terminal           = My.Cfg.terminal cfg
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    , borderWidth        = 1
    , modMask            = mod4Mask
    , workspaces         = wsp
    , normalBorderColor  = "#5b5b5b"
    , focusedBorderColor = "#db7272"
    , keys               = My.Keys.getKeys cfg
    , manageHook
        = manageDocks
        <+> My.Windows.moveWindows wsp
        <+> composeOne
          [ isDialog -?> doCenterFloat
          , stringProperty "WM_WINDOW_ROLE" =? "pop-up" -?> doCenterFloat
          , transience -- Move transient windows to their parent.
          , pure True -?> insertPosition Below Newer
          ]
        <+> namedScratchpadManageHook My.Scratchpad.scratchpads
    , startupHook        = spawn "xfce4-panel --restart"
    , layoutHook         = smartBorders layout
    , handleEventHook    = handleEventHook def <+> docksEventHook
    -- , logHook            = myLogHook
    }

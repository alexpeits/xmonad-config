{-# LANGUAGE CPP #-}
#define JOB
import           XMonad

import           XMonad.Config.Xfce          (xfceConfig)

import           XMonad.Hooks.EwmhDesktops   (ewmh)
import           XMonad.Hooks.InsertPosition (insertPosition, Position(..), Focus(..))
import           XMonad.Hooks.ManageDocks    (manageDocks, docksEventHook)
import           XMonad.Hooks.ManageHelpers  (composeOne, isDialog, (-?>), doCenterFloat, transience)

import           XMonad.Layout.Fullscreen    (fullscreenSupport)
import           XMonad.Layout.NoBorders     (smartBorders)

import           XMonad.Util.NamedScratchpad (namedScratchpadManageHook)

import qualified XMonad.My.Config            as My.Cfg
import qualified XMonad.My.Keys              as My.Keys
import qualified XMonad.My.Layouts           as My.Layouts
import qualified XMonad.My.Scratchpad        as My.Scratchpad
import qualified XMonad.My.Windows           as My.Windows
import qualified XMonad.My.Workspaces        as My.Workspaces


main = do
  let
#ifdef JOB
    cfg = My.Cfg.workHorizontal
#else
    cfg = My.Cfg.thinkpad13
#endif
    wsp = My.Workspaces.getWorkspaces cfg
  xmonad $ fullscreenSupport $ ewmh $ xfceConfig
    { terminal = My.Cfg.terminal cfg
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , borderWidth = 1
    , modMask = mod4Mask
    , workspaces = wsp
    , normalBorderColor = "#5b5b5b"
    , focusedBorderColor = "#db7272"
    , keys = My.Keys.getKeys cfg
    , manageHook =
        manageDocks
        <+> My.Windows.getWindows cfg wsp
        <+> composeOne
          [ isDialog -?> doCenterFloat
          , stringProperty "WM_WINDOW_ROLE" =? "pop-up" -?> doCenterFloat
          , transience -- Move transient windows to their parent.
          , pure True -?> insertPosition Below Newer
          ]
        <+> namedScratchpadManageHook My.Scratchpad.scratchpads
    , startupHook = do
        spawn "xfce4-panel --restart"
        -- setWMName "LG3D"
#ifdef JOB
    , layoutHook = smartBorders My.Layouts.dualScreenHorizontal
#else
    , layoutHook = smartBorders My.Layouts.singleScreen
#endif
    , handleEventHook = handleEventHook def <+> docksEventHook
    }

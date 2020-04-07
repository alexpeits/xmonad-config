{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wunused-imports #-}
import           System.IO                   (hPutStrLn)

import           Network.HostName            (getHostName)

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

main :: IO ()
main = do
  hostname <- getHostName

  cfg <- case lookup hostname My.Cfg.configs of
    Just c -> pure c
    Nothing -> do
      spawn $ unwords
        [ "xmessage '"
        , "No config found for host"
        , hostname
        , "- using default config.'"
        ]
      pure My.Cfg.defaultConfig

  let
    wsp
      = My.Workspaces.workspaces
    -- myLogHook =
      -- when (My.Cfg.useXmobar cfg) $
        -- spawnPipe "xmobar ~/.xmonad/xmobar.hs" >>= xmobarPanel

  xmonad $ fullscreenSupport $ ewmh $ xfceConfig
    { terminal           = My.Cfg.terminal cfg
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    , modMask            = mod4Mask
    , workspaces         = wsp
    , borderWidth        = My.Cfg.borderWidth cfg
    , normalBorderColor  = My.Cfg.normalBorderColor cfg
    , focusedBorderColor = My.Cfg.focusedBorderColor cfg
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
    , layoutHook         = smartBorders (My.Layouts.layout cfg)
    , handleEventHook    = handleEventHook def <+> docksEventHook
    -- , logHook            = myLogHook
    }

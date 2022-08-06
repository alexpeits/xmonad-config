{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wunused-imports #-}

import Control.Monad (unless)
import Data.Maybe (isNothing)
import Network.HostName (getHostName)
import System.Directory (findExecutable)
import System.IO (hPutStrLn)
import XMonad hiding (xmessage)
import qualified XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.InsertPosition (Focus (..), Position (..), insertPosition)
import XMonad.Hooks.ManageDocks (docks, manageDocks)
import XMonad.Hooks.ManageHelpers (composeOne, doCenterFloat, isDialog, transience, (-?>))
import XMonad.Hooks.StatusBar.PP (filterOutWsPP)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.NoBorders (smartBorders)
import qualified XMonad.My.Config as My.Cfg
import qualified XMonad.My.Keys as My.Keys
import qualified XMonad.My.Layouts as My.Layouts
import qualified XMonad.My.Scratchpad as My.Scratchpad
import qualified XMonad.My.Windows as My.Windows
import qualified XMonad.My.Workspaces as My.Workspaces
import XMonad.Util.Loggers.NamedScratchpad as NS.Log
import XMonad.Util.NamedScratchpad as NS
import XMonad.Util.Run (spawnPipe)

xmobarLogHook process =
  DL.dynamicLogWithPP $
    filterOutWsPP ["NSP"] $
      DL.xmobarPP
        { DL.ppOutput = hPutStrLn process . (" " ++),
          DL.ppTitle = DL.xmobarColor "#D08770" "" . DL.shorten 60,
          DL.ppCurrent = DL.xmobarColor "#A3BE8C" "" . DL.wrap "[" "]",
          DL.ppVisible = DL.xmobarColor "#88C0D0" "" . DL.wrap "(" ")",
          DL.ppSep = DL.xmobarColor "#5e7591" "" " | ",
          DL.ppLayout = DL.xmobarColor "#5e7591" "",
          DL.ppUrgent = DL.xmobarColor "red" "yellow",
          DL.ppOrder =
            \case
              [ws, _layout, win] -> [ws, win]
              [ws, _layout, win, nsp] -> [ws, nsp, win]
              other -> other,
          DL.ppExtras = [nspLog]
        }
  where
    nspLog =
      let active = DL.xmobarColor "#A3BE8C" ""
          inactive = DL.xmobarColor "#5e7591" ""
       in fmap DL.trim
            <$> NS.Log.nspActive My.Scratchpad.scratchpadNames active inactive

xmessage :: [String] -> IO ()
xmessage msg = do
  bin <- findExecutable "xmessage"
  unless (isNothing bin) $
    spawn (unwords xmsg)
  where
    xmsg = ["xmessage '"] <> msg <> ["'"]

main :: IO ()
main = do
  hostname <- getHostName

  cfg <- case lookup hostname My.Cfg.configs of
    Just c -> pure c
    Nothing -> do
      xmessage
        [ "No config found for host",
          hostname,
          "- using default config."
        ]
      pure My.Cfg.defaultConfig

  let wsp = My.Workspaces.workspaces
      xmobarConf = My.Cfg.xmobarConf cfg

  xmobarProc <- spawnPipe $ "xmobar " ++ xmobarConf

  xmonad $
    fullscreenSupport $
      docks $
        ewmh $
          def
            { terminal = My.Cfg.terminal cfg,
              focusFollowsMouse = False,
              clickJustFocuses = False,
              modMask = mod4Mask,
              workspaces = wsp,
              borderWidth = My.Cfg.borderWidth cfg,
              normalBorderColor = My.Cfg.normalBorderColor cfg,
              focusedBorderColor = My.Cfg.focusedBorderColor cfg,
              keys = My.Keys.getKeys cfg,
              manageHook =
                manageDocks
                  <+> My.Windows.moveWindows wsp
                  <+> composeOne
                    [ isDialog -?> doCenterFloat,
                      stringProperty "WM_WINDOW_ROLE" =? "pop-up" -?> doCenterFloat,
                      transience, -- Move transient windows to their parent.
                      pure True -?> insertPosition Below Newer
                    ]
                  <+> NS.namedScratchpadManageHook My.Scratchpad.scratchpads,
              startupHook = NS.Log.nspTrackStartup My.Scratchpad.scratchpads,
              layoutHook = smartBorders (My.Layouts.layout cfg),
              handleEventHook =
                handleEventHook def
                  <+> NS.Log.nspTrackHook My.Scratchpad.scratchpads,
              logHook = xmobarLogHook xmobarProc
            }

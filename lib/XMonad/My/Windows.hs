{-# LANGUAGE MultiWayIf      #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module XMonad.My.Windows where

import qualified Data.Map                    as M
import           Data.Word                   (Word32)

import           XMonad
import qualified XMonad.StackSet             as W

import           XMonad.Hooks.ManageHelpers  (doFloatAt)
import           XMonad.Util.NamedScratchpad (customFloating)


nWorkspace :: [String] -> Int -> String
nWorkspace ws i = ws !! (i - 1)

moveWindows _wsp
  = composeAll
      [ className =? "Gnome-calculator" --> smallRectTR
      , className =? "Indicator.py"     --> doFloatAt 0.43 0.43
      , className =? "Zenity"           --> doFloatAt 0.43 0.43
      , className =? "Gcolor3"          --> doFloat
      , className =? "stalonetray"      --> doIgnore
      , className =? "Xfce4-notifyd"    --> doIgnore
      ]

-- myTopMargin = 28 / 2160
myTopMargin = 0

middleRR   w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h
topRightRR w h = W.RationalRect (1 - w) myTopMargin w h
topLeftRR  w h = W.RationalRect 0 myTopMargin w h
botRightRR w h = W.RationalRect (1 - w) (1 - h) w h
botLeftRR  w h = W.RationalRect 0 (1 - h) w h
dropDownRR w h = W.RationalRect 0 myTopMargin w h

largeRectM  = customFloating $ middleRR 0.8 0.8
medRectM    = customFloating $ middleRR 0.65 0.75
medRectBR   = customFloating $ botRightRR 0.45 0.5
smallRectM  = customFloating $ middleRR 0.45 0.55
smallRectTR = customFloating $ topRightRR 0.25 0.3
smallRectBR = customFloating $ botRightRR 0.3 0.4
dropDown    = customFloating $ dropDownRR 1 0.35

isFloatQ :: Query Bool
isFloatQ = ask >>= \w -> (liftX . gets) (M.member w . W.floating . windowset)

cycleCorner :: Window -> X ()
cycleCorner w = do
  isFloat <- runQuery isFloatQ w
  if isFloat then go else pure ()
  where
    go = withDisplay $ \d -> do
      border <- asks (fromIntegral . borderWidth . config)
      scrRect <- gets (screenRect . W.screenDetail . W.current . windowset)
      wa <- io $ getWindowAttributes d w
      let
        clamp lim a
          | a < 0     = 0
          | a > lim   = lim
          | otherwise = a

        clampW = clamp sw
        clampH = clamp sh

        wx = fromIntegral $ wa_x wa
        wy = fromIntegral $ wa_y wa

        ww = fromIntegral $ wa_width wa
        wh = fromIntegral $ wa_height wa

        sw = fromIntegral $ rect_width scrRect
        sh = fromIntegral $ rect_height scrRect

        bw = 2 * border

        topLeft  = (0, 0)
        topRight = (clampW $ sw - ww - bw, 0)
        botLeft  = (0, clampH $ sh - wh - bw)
        botRight = (clampW $ sw - ww - bw, clampH $ sh - wh - bw)

        (newX, newY)
          = if
          | (wx, wy) == botRight -> botLeft
          | (wx, wy) == botLeft  -> topLeft
          | (wx, wy) == topLeft  -> topRight
          | otherwise            -> botRight

      io $ moveWindow d w newX newY

      float w

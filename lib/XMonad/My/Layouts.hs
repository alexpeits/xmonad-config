{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module XMonad.My.Layouts where

import           XMonad                           (Window, def, (|||))

import           XMonad.Config.Prime              (LayoutClass)

import qualified XMonad.Layout                    as XL
import qualified XMonad.Layout.Decoration         as Deco
import qualified XMonad.Layout.Dwindle            as Dwindle
import qualified XMonad.Layout.Maximize           as Maximize
import           XMonad.Layout.MultiToggle        ((??))
import qualified XMonad.Layout.MultiToggle        as MultiToggle
import qualified XMonad.Layout.NoFrillsDecoration as NoFrills
import qualified XMonad.Layout.Reflect            as Reflect
import qualified XMonad.Layout.ResizableTile      as ResizableTile
import qualified XMonad.Layout.StackTile          as StackTile
import qualified XMonad.Layout.Tabbed             as Tabbed

import           XMonad.Hooks.ManageDocks         (avoidStruts)

layout
  = avoidStruts $ tall ||| focus ||| tabbed
  where

    tall
      = withToggle
      $ withMaximize
      $ ResizableTile.ResizableTall 1 (3/100) 0.5 []

    left
      = withToggle
      $ withMaximize
      $ XL.Tall 1 (3/100) 0.75

    right
      = withToggle
      $ withMaximize
      $ Reflect.reflectHoriz
      $ XL.Tall 1 (3/100) 0.75

    spiralRight
      = withToggle
      $ withMaximize
      $ Dwindle.Dwindle Dwindle.L Dwindle.CCW 2.75 1.07

    focus
      = withToggle
      $ withMaximize
      $ XL.Mirror left

    stackTile
      = withToggle
      $ withMaximize
      $ StackTile.StackTile 2 (3/100) 0.7

    tabbed
      = Tabbed.tabbed Deco.shrinkText tabTheme
      where
        tabTheme
          = def { Tabbed.activeColor = "#245361"
                , Tabbed.activeBorderColor = "#245361"
                , Tabbed.inactiveColor = "#091f2e"
                , Tabbed.inactiveBorderColor = "#245361"
                , Tabbed.fontName = "xft:Monospace:size=8"
                , Tabbed.decoHeight = 18
                }

    -- withTopBar
    --   = NoFrills.noFrillsDeco Deco.shrinkText topBarTheme
    --   where
    --     topBarTheme
    --       = def { NoFrills.inactiveBorderColor   = "#002b36"
    --             , NoFrills.inactiveColor         = "#002b36"
    --             , NoFrills.inactiveTextColor     = "#002b36"
    --             , NoFrills.activeBorderColor     = "#268bd2"
    --             , NoFrills.activeColor           = "#268bd2"
    --             , NoFrills.activeTextColor       = "#268bd2"
    --             , NoFrills.decoHeight            = 10
    --             }

    withMaximize
      :: LayoutClass l Window
      => l Window
      -> Deco.ModifiedLayout Maximize.Maximize l Window
    withMaximize
      = Maximize.maximizeWithPadding 10

    withToggle
      :: LayoutClass l a
      => l a
      -> MultiToggle.MultiToggle
           (MultiToggle.HCons Reflect.REFLECTX
             (MultiToggle.HCons Reflect.REFLECTY MultiToggle.EOT))
           l a
    withToggle
      = MultiToggle.mkToggle
          (Reflect.REFLECTX ?? Reflect.REFLECTY ?? MultiToggle.EOT)

    withVerticalToggle
      :: LayoutClass l a
      => l a
      -> MultiToggle.MultiToggle
           (MultiToggle.HCons Reflect.REFLECTY MultiToggle.EOT)
           l a
    withVerticalToggle
      = MultiToggle.mkToggle (MultiToggle.single Reflect.REFLECTY)

    withHorizontalToggle
      :: LayoutClass l a
      => l a
      -> MultiToggle.MultiToggle
           (MultiToggle.HCons Reflect.REFLECTX MultiToggle.EOT)
           l a
    withHorizontalToggle
      = MultiToggle.mkToggle (MultiToggle.single Reflect.REFLECTX)

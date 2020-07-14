{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wunused-imports #-}
module XMonad.My.Layouts where

import           GHC.Word                         (Word64)

import           XMonad
import           XMonad.Config.Prime              (LayoutClass(..))

import           XMonad.Hooks.ManageDocks         (avoidStruts)

import qualified XMonad.Layout.IfMaxFix           as IfMax

import qualified XMonad.Layout                    as XL
import qualified XMonad.Layout.Decoration         as Deco
import qualified XMonad.Layout.Dwindle            as Dwindle
import qualified XMonad.Layout.Maximize           as Maximize
import           XMonad.Layout.MultiToggle        ((??))
import qualified XMonad.Layout.MultiToggle        as MultiToggle
import           XMonad.Layout.Named              (named)
import qualified XMonad.Layout.NoFrillsDecoration as NoFrills
import qualified XMonad.Layout.Reflect            as Reflect
import qualified XMonad.Layout.ResizableTile      as ResizableTile
import qualified XMonad.Layout.StackTile          as StackTile
import qualified XMonad.Layout.Tabbed             as Tabbed
import qualified XMonad.Layout.ToggleLayouts      as ToggleLayouts

import qualified XMonad.My.Config                 as Cfg

inactiveColor = "#002b36"

layout cfg
  = avoidStruts $ tall ||| focus ||| tabbed
  where
    tall
      = named "tall"
      $ tiled
      $ ResizableTile.ResizableTall 1 (3/100) 0.5 []

    focus
      = named "focus"
      $ tiled
      $ XL.Mirror baseLeft

    stackTile
      = named "stack"
      $ tiled
      $ StackTile.StackTile 2 (3/100) 0.7

    spiral
      = named "spiral"
      $ tiled
      $ Dwindle.Dwindle Dwindle.R Dwindle.CW 1.0 1.07

    tabbed
      = named "tab"
      $ Tabbed.tabbed Deco.shrinkText tabTheme
      where
        tabTheme
          = def
              { Tabbed.activeColor         = Cfg.tabColor cfg
              , Tabbed.activeBorderColor   = Cfg.tabColor cfg
              , Tabbed.inactiveColor       = inactiveColor
              , Tabbed.inactiveBorderColor = Cfg.tabColor cfg
              , Tabbed.fontName            = "xft:Monospace:size=8"
              , Tabbed.decoHeight          = 18
              }

    baseLeft
      = ResizableTile.ResizableTall 1 (3/100) 0.75 []

    baseRight
      = Reflect.reflectHoriz baseLeft

    -- as opposed to `tabbed`
    tiled
      :: LayoutClass l Word64
      => l Window
      -> MultiToggle.MultiToggle
           (MultiToggle.HCons
             Reflect.REFLECTX
             (MultiToggle.HCons Reflect.REFLECTY MultiToggle.EOT))
           (Deco.ModifiedLayout
             Maximize.Maximize
             (IfMax.IfMax
               l
               (ToggleLayouts.ToggleLayouts
                 l
                 (Deco.ModifiedLayout
                   (Deco.Decoration NoFrills.NoFrillsDecoration Deco.DefaultShrinker)
                   l))))
           Window
    tiled l
      = withReflect
      $ withMaximize
      $ IfMax.ifMax 1 l (ToggleLayouts.toggleLayouts l (withTopBar l))

    withTopBar
      :: l Window
      -> Deco.ModifiedLayout
           (Deco.Decoration NoFrills.NoFrillsDecoration Deco.DefaultShrinker)
           l
           Window
    withTopBar
      = NoFrills.noFrillsDeco Deco.shrinkText topBarTheme
      where
        topBarTheme
          = def { NoFrills.inactiveBorderColor   = inactiveColor
                , NoFrills.inactiveColor         = inactiveColor
                , NoFrills.inactiveTextColor     = inactiveColor
                , NoFrills.activeBorderColor     = Cfg.focusedBorderColor cfg
                , NoFrills.activeColor           = Cfg.focusedBorderColor cfg
                , NoFrills.activeTextColor       = Cfg.focusedBorderColor cfg
                , NoFrills.decoHeight            = Cfg.topBarHeight cfg
                }

    withMaximize
      :: LayoutClass l Window
      => l Window
      -> Deco.ModifiedLayout Maximize.Maximize l Window
    withMaximize
      = Maximize.maximizeWithPadding 0

    withReflect
      :: LayoutClass l a
      => l a
      -> MultiToggle.MultiToggle
           (MultiToggle.HCons Reflect.REFLECTX
             (MultiToggle.HCons Reflect.REFLECTY MultiToggle.EOT))
           l a
    withReflect
      = MultiToggle.mkToggle
          (Reflect.REFLECTX ?? Reflect.REFLECTY ?? MultiToggle.EOT)

module XMonad.My.Layouts where

import           XMonad                           (def, (|||))

import           XMonad.Config.Prime              (LayoutClass)

import qualified XMonad.Layout                    as XL
import qualified XMonad.Layout.Decoration         as Deco
import qualified XMonad.Layout.Dwindle            as Dwindle
import qualified XMonad.Layout.Maximize           as Maximize
import qualified XMonad.Layout.MultiToggle        as MultiToggle
import           XMonad.Layout.Named              (named)
import qualified XMonad.Layout.NoFrillsDecoration as NoFrills
import qualified XMonad.Layout.Reflect            as Reflect
import qualified XMonad.Layout.Simplest           as Simplest
import qualified XMonad.Layout.StackTile          as StackTile
import qualified XMonad.Layout.Tabbed             as Tabbed

import           XMonad.Hooks.ManageDocks         (avoidStruts)


(singleScreen, dualScreenHorizontal)
  = ( avoidStruts $ Maximize.maximizeWithPadding 40 singleScreenLayout
    , avoidStruts $ Maximize.maximizeWithPadding 40 dualScreenHorizontalLayout
    )
  where
    singleScreenLayout
      =   tall
      ||| focus
      ||| tabbed

    dualScreenHorizontalLayout
      =   withTopBar tall
      ||| withTopBar spiralRight
      ||| withTopBar focus
      ||| withTopBar stackTile

    tall
      = withHorizontalToggle
        $ XL.Tall 1 (3/100) 0.5

    left
      = XL.Tall 1 (3/100) 0.75

    right
      = withHorizontalToggle
        $ Reflect.reflectHoriz
        $ XL.Tall 1 (3/100) 0.75

    spiralRight
      = withHorizontalToggle
        $ Dwindle.Dwindle Dwindle.L Dwindle.CCW 2.75 1.07

    focus
      = XL.Mirror left

    stackTile
      = StackTile.StackTile 2 (3/100) 0.7

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

    withTopBar
      = NoFrills.noFrillsDeco Deco.shrinkText topBarTheme
      where
        topBarTheme
          = def { NoFrills.inactiveBorderColor   = "#002b36"
                , NoFrills.inactiveColor         = "#002b36"
                , NoFrills.inactiveTextColor     = "#002b36"
                , NoFrills.activeBorderColor     = "#268bd2"
                , NoFrills.activeColor           = "#268bd2"
                , NoFrills.activeTextColor       = "#268bd2"
                , NoFrills.decoHeight            = 10
                }

    withHorizontalToggle
      :: LayoutClass l a
      => l a
      -> MultiToggle.MultiToggle
           (MultiToggle.HCons Reflect.REFLECTX MultiToggle.EOT)
           l a
    withHorizontalToggle
      = MultiToggle.mkToggle (MultiToggle.single Reflect.REFLECTX)

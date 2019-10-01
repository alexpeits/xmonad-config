{-# LANGUAGE RecordWildCards #-}
module XMonad.My.Workspaces where

import           Data.List                        (sort)

import           XMonad.Layout.IndependentScreens (withScreens)

import qualified XMonad.My.Config                 as Cfg


singleScreen :: [String]
singleScreen
  = [ "1:main"
    , "2:term"
    , "3:emacs"
    , "4:chat"
    , "5:music"
    , "6:vm"
    ]

dualScreen :: [String]
dualScreen
  = sort $ withScreens 2 $ map show [1..4]

getWorkspaces :: Cfg.Config -> [String]
getWorkspaces Cfg.Config{..}
  = case screens of
      Cfg.SingleScreen _ -> singleScreen
      Cfg.DualScreenHorizontal _ _ -> dualScreen

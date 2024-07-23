module Config.Layout where

import XMonad hiding ((|||))
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing

myLayout = (gaps [(L,4), (R,4), (D,4)] $ spacing 4 $ avoidStruts $ tiled ||| reflectHoriz tiled ||| Mirror tiled) ||| noBorders Full
  where
    tiled = ResizableTall 1 (3/100) (2/3) []

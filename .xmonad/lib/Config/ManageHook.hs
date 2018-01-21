module Config.ManageHook where

import XMonad
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W

myManageHook = composeOne
    [ appName =? "floatingTerminal" -?> doRectFloat (W.RationalRect 0.3 0.35 0.4 0.35)
    , appName =? "slaveTerminal" -?> doF (W.swapDown)
    , isFullscreen -?> doFullFloat
    ] 

module Config.ManageHook where

import XMonad
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W

myManageHook = composeOne
    [ appName =? "floatingTerminal" -?> doRectFloat (W.RationalRect 0.3 0.35 0.4 0.35)
    , title =? "prose" -?> doRectFloat (W.RationalRect 0.3 0.30 0.4 0.40)
    -- TODO Doesn't seem to work
    , appName =? "Game" -?> doRectFloat (W.RationalRect 0.55 0.04 0.44 0.6)
    , appName =? "Alchemy Brawl (DEBUG)" -?> doRectFloat (W.RationalRect 0.55 0.04 0.44 0.6)
    , appName =? "slaveTerminal" -?> doF (W.swapDown)
    , isFullscreen -?> doFullFloat
    ] 

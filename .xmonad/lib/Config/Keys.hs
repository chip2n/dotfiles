module Config.Keys where

import XMonad hiding ((|||))
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ResizableTile
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import System.Exit
import Config.Theme

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, xK_Return), spawn $ (XMonad.terminal conf) ++ " -name slaveTerminal" )
    , ((modMask .|. shiftMask, xK_Return), spawn $ (XMonad.terminal conf) ++ " -name floatingTerminal" )
    , ((modMask, xK_BackSpace), spawn "emacsclient -c" )
    , ((modMask, xK_p), spawn $ "dmenu_run -h " ++ show barHeight ++" -nb '" ++ colorBarBg ++ "'"  )
    ]
    ++
    layoutKeys conf
    ++
    [ ((modMask .|. shiftMask, xK_c), kill)
    , ((modMask, xK_comma), sendMessage (IncMasterN 1))
    , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask, xK_t), withFocused $ windows . W.sink)
    , ((modMask, xK_Tab), toggleWS)
    , ((modMask .|. shiftMask .|. controlMask, xK_j), shiftNextScreen)

    -- quit or restart
    , ((modMask, xK_q), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart")
    , ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    ]
    ++
    -- workspace switch
    zipWith (\w k -> ((modMask, k), windows (W.greedyView w))) (XMonad.workspaces conf) [xK_1 .. xK_9]
    ++
    -- workspace switch (multihead swapping)
    --zipWith (\w k -> ((modMask .|. shiftMask .|. controlMask, k), windows (W.greedyView w))) (XMonad.workspaces conf) [xK_1 .. xK_9]
    -- ++
    -- workspace window move
    zipWith (\w k -> ((modMask .|. shiftMask, k), windows (W.shift w))) (XMonad.workspaces conf) [xK_1 .. xK_9]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

layoutKeys conf@(XConfig {XMonad.modMask = modMask}) =
    [ ((modMask, xK_space), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_f), sendMessage $ JumpToLayout "Full")
    , ((modMask, xK_h), sendMessage Shrink)
    , ((modMask, xK_l), sendMessage Expand)
    , ((modMask, xK_minus), sendMessage MirrorShrink)
    , ((modMask, xK_plus), sendMessage MirrorExpand)
    , ((modMask, xK_j), windows W.focusDown)
    , ((modMask, xK_k), windows W.focusUp)
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
    ]

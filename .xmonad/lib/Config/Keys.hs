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
    [ ((modMask, xK_Return), spawn $ (XMonad.terminal conf) ++ " -n slaveTerminal" )
    , ((modMask .|. shiftMask, xK_Return), spawn $ (XMonad.terminal conf) ++ " -n floatingTerminal" )
    , ((modMask, xK_BackSpace), spawn "emacsclient -c" )
    , ((modMask, xK_p), spawn $ "dmenu_run -h " ++ show barHeight ++" -nb '" ++ colorBarBg ++ "'"  )
    ]
    ++
    workspaceBindings conf
    ++
    layoutBindings conf
    ++
    [ ((modMask, xK_c), kill)
    , ((modMask, xK_comma), sendMessage (IncMasterN 1))
    , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask, xK_t), withFocused $ windows . W.sink)
    , ((modMask, xK_Tab), toggleWS)
    , ((modMask .|. shiftMask .|. controlMask, xK_j), shiftNextScreen)

    -- quit or restart
    , ((modMask .|. shiftMask, xK_q), spawn "xmonad --recompile && xmonad --restart")
    , ((modMask .|. shiftMask .|. controlMask, xK_q), io (exitWith ExitSuccess))
    ]
    ++
    -- mod-{q,w,f}        Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{q,w,f}  Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_q, xK_w, xK_f] [0..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

workspaceBindings conf@(XConfig {XMonad.modMask = modMask,
                                 XMonad.workspaces = workspaces}) =
    let windowSwitchKeys = map (\k -> (modMask, k)) [xK_1 .. xK_9]
        windowMoveKeys   = map (\k -> ((modMask .|. shiftMask), k)) [xK_1 .. xK_9]
        buildWorkspaceBindings keys f = zipWith (\k w -> (k, f w)) keys workspaces
    in (buildWorkspaceBindings windowSwitchKeys $ windows . W.greedyView)
       ++
       (buildWorkspaceBindings windowMoveKeys $ windows . W.shift)

layoutBindings conf@(XConfig {XMonad.modMask = modMask}) =
    [ ((modMask, xK_space), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    --, ((modMask, xK_f), sendMessage $ JumpToLayout "Full")
    , ((modMask, xK_h), sendMessage Shrink)
    , ((modMask, xK_l), sendMessage Expand)
    , ((modMask .|. controlMask, xK_j), sendMessage MirrorShrink)
    , ((modMask .|. controlMask, xK_k), sendMessage MirrorExpand)
    , ((modMask, xK_j), windows W.focusDown)
    , ((modMask, xK_k), windows W.focusUp)
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)

    , ((modMask, xK_i), sendMessage Expand)
    , ((modMask .|. controlMask, xK_n), sendMessage MirrorShrink)
    , ((modMask .|. controlMask, xK_e), sendMessage MirrorExpand)
    , ((modMask, xK_n), windows W.focusDown)
    , ((modMask, xK_e), windows W.focusUp)
    , ((modMask .|. shiftMask, xK_n), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_e), windows W.swapUp)
    ]

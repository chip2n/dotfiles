import XMonad hiding ((|||))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import System.Exit

colorNormalBorder = "#3c404a"
colorFocusedBorder = "#898f9c"
colorInactive = "#77838c"
colorActive = "#dfdfdf"
colorBarBg = "#21242b"

barHeight = 32
barFont = "terminus"

emptyWorkspaceIcon = "\xE0E6"
fullWorkspaceIcon = "\xE056"

main = do
    --dbus <- initDBus
    spawnPipe "launch_polybar.sh"
    xmonad myConfig

myConfig = ewmh . docks $ def
    { terminal          = "st"
    , workspaces        = ["1", "2", "3", "4", "5"]
    , focusFollowsMouse = False

    , borderWidth        = 1
    , normalBorderColor  = colorNormalBorder
    , focusedBorderColor = colorFocusedBorder

    , modMask = mod4Mask
    , keys    = myKeys

    , layoutHook      = myLayout
    , manageHook      = myManageHook
    , logHook         = myLogHook
    }

myLayout = (gaps [(L,4), (R,4), (D,4)] $ spacing 4 $ avoidStruts $ tiled ||| reflectHoriz tiled ||| Mirror tiled) ||| noBorders Full
  where
    tiled = ResizableTall 1 (3/100) (2/3) []

myManageHook = composeOne
    [ appName =? "floatingTerminal" -?> doRectFloat (W.RationalRect 0.3 0.35 0.4 0.35)
    , title =? "prose" -?> doRectFloat (W.RationalRect 0.3 0.30 0.4 0.40)
    -- TODO Doesn't seem to work
    , appName =? "Game" -?> doRectFloat (W.RationalRect 0.55 0.04 0.44 0.6)
    , appName =? "Alchemy Brawl (DEBUG)" -?> doRectFloat (W.RationalRect 0.55 0.04 0.44 0.6)
    , appName =? "slaveTerminal" -?> doF (W.swapDown)
    , isFullscreen -?> doFullFloat
    ]

--myLogHook :: D.Client -> X ()
--myLogHook = dynamicLogWithPP . logPP
myLogHook :: X ()
myLogHook = dynamicLogWithPP logPP

--logPP :: D.Client -> PP
--logPP dbus = dbusPP dbus $ def {
logPP :: PP
logPP = def {
      ppLayout = \x -> ""
    , ppExtras = []
    , ppTitle = \x -> ""
    , ppHidden = \x -> wrapForegroundColor colorInactive fullWorkspaceIcon
    , ppHiddenNoWindows = \x -> wrapForegroundColor colorInactive emptyWorkspaceIcon
    , ppCurrent = \x -> wrapForegroundColor colorActive fullWorkspaceIcon
    , ppVisible = \x -> wrapForegroundColor colorActive fullWorkspaceIcon
    , ppSep = ""
  }

wrapForegroundColor color s = wrap fgStart fgReset s
  where fgStart = "%{F"  ++ color ++ "}"
        fgReset = "%{F-}"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, xK_Return), spawn $ (XMonad.terminal conf) ++ " -n slaveTerminal" )
    , ((modMask .|. shiftMask, xK_Return), spawn $ (XMonad.terminal conf) ++ " -n floatingTerminal" )
    , ((modMask, xK_BackSpace), spawn "emacsclient -c" )
    , ((modMask, xK_semicolon), spawn "prose" )
    , ((modMask, xK_p), spawn $ "rofi -show run") -- dmenu_run -h " ++ show barHeight ++" -nb '" ++ colorBarBg ++ "'"  )
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
    [((modMask, key), viewScreen def sc) | (key, sc) <- zip [xK_q, xK_w, xK_f] [0..]]
    ++
    [((modMask .|. shiftMask, key), sendToScreen def sc) | (key, sc) <- zip [xK_q, xK_w, xK_f] [0..]]

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
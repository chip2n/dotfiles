import XMonad hiding ((|||))
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Layout.Spacing
import System.Exit

--main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
main = do
    b <- statusBar myBar myPP toggleStrutsKey myConfig
    spawnPipe "~/.xmonad/launch_conky.sh"
    spawnPipe "~/.xmonad/launch_trayer.sh"
    xmonad b

-- Status bar {{{
myBar = "dzen2 -dock -fn terminus-8 -x '0' -y '0' -h '" ++ show barHeight ++ "' -w '1920' -ta 'l' -fg '#ffffff' -bg '" ++ colorBarBg ++ "' -xs 1"
myBitmapsDir = "/home/chip/.xmonad/icons/"

myPP = defaultPP
    {
        ppCurrent = dzenColor colorFocusedBorder colorBarBg . pad
      , ppVisible = dzenColor "white" colorBarBg . pad
      , ppHidden = dzenColor "white" colorBarBg . pad
      , ppHiddenNoWindows = dzenColor colorNormalBorder colorBarBg . pad
      , ppSep = pad " | "
      , ppOrder = \(workspaces:layout:title:xs) -> (layout:workspaces:title:xs)
      , ppLayout = dzenColor colorFocusedBorder colorBarBg . (" " ++) . pad .
                   (\x -> case x of
                       "Spacing 4 ResizableTall" -> "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                       "Spacing 4 ReflectX ResizableTall" -> "^i(" ++ myBitmapsDir ++ "/rtall.xbm)"
                       "Spacing 4 Mirror ResizableTall" -> "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                       "Full" -> "^i(" ++ myBitmapsDir ++ "/full2.xbm)"
                       _               -> x
                   )
    }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
--}}}

workspaceIcons = map diceIcon [1..5]
    where diceIcon s = "^i(" ++ myBitmapsDir ++ "dice" ++ show s ++ ".xbm)"
myWorkspaces = workspaceIcons

myConfig = desktopConfig
    { terminal    = "urxvt"
    , workspaces  = myWorkspaces
    , modMask     = mod4Mask
    , keys        = myKeys
    , borderWidth = 1
    , normalBorderColor = colorNormalBorder
    , focusedBorderColor = colorFocusedBorder
    , layoutHook = myLayout
    , manageHook = myManageHook
    , handleEventHook = docksEventHook
    , focusFollowsMouse = False
    }

--myLayout = gaps [(L,8), (U,8), (R,8), (D,8)] $ avoidStruts $ tiled ||| reflectHoriz tiled ||| Mirror tiled ||| noBorders Full
myLayout = (gaps [(L,4), (R,4), (D,4)] $ spacing 4 $ avoidStruts $ tiled ||| reflectHoriz tiled ||| Mirror tiled) ||| noBorders Full
  where
    tiled = ResizableTall 1 (3/100) (2/3) []

myManageHook = composeOne [
      className =? "floatingTerminal" -?> doRectFloat (W.RationalRect 0.3 0.35 0.4 0.35),
      className =? "slaveTerminal" -?> insertPosition Above Older,
      --className =? "floatingTerminal" -?> doFloat,
      --className =? "slaveTerminal" -?> doF (W.swapDown),
      isFullscreen -?> doFullFloat
    ] 
        

-- Theme {{{
-- Colors
colorNormalBorder = "#153b47"
colorFocusedBorder = "#3a8ba6"

--colorBarBg = "#021d1f"
colorBarBg = "#21242b"
barHeight = 24

barFont = "terminus"
--}}}

-- Key bindings {{{
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, xK_Return), spawn $ (XMonad.terminal conf) ++ " -name slaveTerminal" )
    , ((modMask .|. shiftMask, xK_Return), spawn $ (XMonad.terminal conf) ++ " -name floatingTerminal" )
    , ((modMask, xK_p), spawn $ "dmenu_run -h " ++ show barHeight ++" -nb '" ++ colorBarBg ++ "'"  )

    -- layouts
    , ((modMask, xK_space), sendMessage NextLayout)
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
    --, ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_c), kill)
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
-- }}}

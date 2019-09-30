import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run
import Config.Theme
import Config.Keys
import Config.DBus
import Config.LogHook    (myLogHook)
import Config.ManageHook (myManageHook)
import Config.Layout     (myLayout)

main = do
    dbus <- initDBus
    spawnPipe "~/scripts/launch_polybar.sh"
    xmonad $ myConfig dbus

myConfig dbus = ewmh . docks $ def
    { terminal          = "urxvt"
    , workspaces        = ["1", "2", "3", "4", "5"]
    , focusFollowsMouse = False

    , borderWidth        = 1
    , normalBorderColor  = colorNormalBorder
    , focusedBorderColor = colorFocusedBorder

    , modMask = mod4Mask
    , keys    = myKeys

    , layoutHook      = myLayout
    , manageHook      = myManageHook
    , logHook         = myLogHook dbus
    , handleEventHook = docksEventHook
    }

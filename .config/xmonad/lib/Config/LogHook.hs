module Config.LogHook where

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
--import qualified DBus.Client as D
--import Config.DBus
import Config.Theme

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

{-# LANGUAGE OverloadedStrings, PackageImports #-}

import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.ManageHook
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doFullFloat, isInProperty)
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Submap
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import qualified XMonad.StackSet as W
import qualified "dbus" DBus as D
import qualified "dbus" DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import XMonad.Util.EZConfig(additionalKeysP, additionalKeys)
import XMonad.Util.CustomKeys
import XMonad.Util.Run(spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import System.IO
import Keys(myKeys, mySubmap)

--myNormalBorderColor     = "#dddddd"
--myFocusedBorderColor    = "#ffffff"

myManageHook = composeAll
    [ className =? "MPlayer"        --> (doFloat <+> doShift "5")
    , className =? "mplayer2"       --> (doFloat <+> doShift "5")
    , className =? "Firefox"        --> doShift "2"
    , className =? "Opera"          --> doShift "2"
    , className =? "Zim"            --> doShift "8"
    , className =? "eclipse"        --> doShift "3"
    , className =? "feh"            --> doShift "5"
    , className =? "Inkscape"       --> doShift "6"
    , className =? "Blender"        --> doShift "6"
    , className =? "Thunderbird"    --> doShift "4"
    , title     =? "JDownloader"    --> doShift "9"
    , className =? "xpdf"           --> doShift "8"
    , className =? "nautilus"       --> doFloat
    , className =? "hl2_linux"      --> doFullFloat
    , className =? "Steam"          --> doFullFloat
    , isModal			    --> doFloat	
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    ] <+> manageHook gnomeConfig

myLogHook h = (dynamicLogWithPP $ defaultPP
  { ppCurrent         = dzenColor "#303030" "#909090" . pad
  , ppHidden          = dzenColor "#909090" "" . pad
  , ppHiddenNoWindows = dzenColor "#606060" "" . pad
  , ppLayout          = dzenColor "#909090" "" . pad
  , ppUrgent          = wrap (dzenColor "#2E8B57" "" "<") (dzenColor "#2E8B57" "" ">") . pad
  , ppTitle           = wrap "^fg(#909090)[ " " ]^fg()" . shorten 40 
  , ppWsSep           = ""
  , ppSep             = "  "
  , ppOutput          = hPutStrLn h -- ... must match the h here
  }) >> updatePointer (Relative 0.95 0.95) <+> setWMName "LG3D"

isModal :: Query Bool
isModal = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_MODAL"

main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ withUrgencyHook NoUrgencyHook $ gnomeConfig
        { 
            terminal            = "gnome-terminal",
            focusFollowsMouse   = True,
            borderWidth         = 1,
            modMask             = mod4Mask,
--            normalBorderColor   = myNormalBorderColor,
--            focusedBorderColor  = myFocusedBorderColor,
            manageHook          = manageDocks <+> myManageHook <+> fullscreenManageHook,
            layoutHook          = (fullscreenFloat . fullscreenFull) $ avoidStruts $ smartBorders (layoutHook gnomeConfig),
            handleEventHook     = handleEventHook gnomeConfig <+> fullscreenEventHook,
            logHook             = dynamicLogWithPP (prettyPrinter dbus),
            startupHook         = setWMName "LG3D"
        } `additionalKeysP` myKeys `additionalKeys` mySubmap

-- xmonad-log-applet hook

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput = dbusOutput dbus
    , ppTitle = pangoSanitize
    , ppCurrent = pangoColor "white" . wrap "[" "]" . pangoSanitize
    , ppVisible = pangoColor "grey" . wrap "(" ")" . pangoSanitize
    , ppHidden = pangoColor "grey"              
    , ppUrgent = pangoColor "red"
    , ppLayout = pangoColor "white"
    , ppSep = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
 where
  left = "<span foreground=\"" ++ fg ++ "\">"
  right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>' xs = "&gt;" ++ xs
    sanitize '<' xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&' xs = "&amp;" ++ xs
    sanitize x xs = x:xs


import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.ManageHook
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doFullFloat)
import XMonad.Hooks.ICCCMFocus
import XMonad.Actions.UpdatePointer
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.CustomKeys
import XMonad.Util.Run(spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import System.IO
import Keys(myKeys)

myNormalBorderColor     = "#dddddd"
myFocusedBorderColor    = "#ffffff"

myManageHook = composeAll
    [ className =? "MPlayer"        --> (doFloat <+> doShift "5")
    , className =? "Firefox"        --> doShift "2"
    , className =? "Opera"          --> doShift "2"
    , className =? "zim"            --> doShift "8"
    , className =? "eclipse"        --> doShift "3"
    , className =? "feh"            --> doShift "5"
    , className =? "Inkscape"       --> doShift "6"
    , className =? "Blender"        --> doShift "6"
    , className =? "xpdf"           --> doShift "8"
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    ] <+> manageHook defaultConfig

myLayoutHook = avoidStruts $ smartBorders (
    Tall 1 (3/100) (1/2) ||| 
    Mirror (Tall 1 (3/100) (1/2)) ||| 
    Full)

myEventHook = ewmhDesktopsEventHook

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
  }) >> updatePointer (Relative 0.95 0.95) <+> takeTopFocus >> setWMName "LG3D"

dzenStyle   = " -fn -*-verdana-medium-r-normal--11-*-*-*-*-*-*-* -y 0 -w 960 -h 15 -fg '#ffffff' -bg '#0e0e0e' -e 'onexit=ungrabmouse'"
myStatusBar = "/usr/local/bin/dzen2 -p -ta l -x 0" ++ dzenStyle
myOtherBar  = "conky -c ~/.xmonad/conky_dzen | /usr/local/bin/dzen2 -p -ta r -x 960" ++ dzenStyle

main = do
    d <- spawnPipe myStatusBar
    spawn myOtherBar

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { 
            terminal            = "urxvtc",
            focusFollowsMouse   = True,
            borderWidth         = 1,
            modMask             = mod4Mask,
            normalBorderColor   = myNormalBorderColor,
            focusedBorderColor  = myFocusedBorderColor,
            manageHook          = manageDocks <+> myManageHook,
            logHook             = myLogHook d,
            layoutHook          = myLayoutHook,
            startupHook         = setWMName "LG3D"
        } `additionalKeysP` myKeys

module Keys where
  import XMonad
  import Graphics.X11.ExtraTypes.XF86
  import XMonad.Prompt
  import XMonad.Prompt.RunOrRaise

  myKeys :: [(String, X())]  
  myKeys = [ ("M-p", spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- dmenu
           , ("<XF86AudioMute>", spawn "amixer -q set PCM toggle")
           , ("<XF86AudioRaiseVolume>", spawn "amixer -q set PCM 5+")
           , ("<XF86AudioLowerVolume>", spawn "amixer -q set PCM 5-")
           , ("M-i", spawn "notedpp -l | dmenu -l 10")
           , ("M-o", spawn "noted_helper -n")
           , ("M-ü", spawn "noted_helper -d")
           , ("M-S-l", spawn "xscreensaver-command -lock && xset dpms force off") -- lock workstation and turn off display
           , ("M-x", runOrRaisePrompt defaultXPConfig) -- Run or Raise
           , ("<XF86AudioNext>", spawn "ncmpcpp next")
           , ("<XF86AudioPrev>", spawn "ncmpcpp prev")
           , ("<XF86AudioPlay>", spawn "ncmpcpp toggle")
           , ("<XF86AudioStop>", spawn "ncmpcpp stop")
           ]

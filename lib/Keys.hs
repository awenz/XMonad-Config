module Keys where
  import XMonad
  import Graphics.X11.ExtraTypes.XF86
  import XMonad.Prompt
  import XMonad.Prompt.RunOrRaise
  import XMonad.Actions.Submap
  import qualified Data.Map as M

  myKeys :: [(String, X())]  
  myKeys = [ ("M-p", spawn "x=$(yeganesh -x -- -i -fn Verdana-12) && exec $x") -- dmenu
           , ("<XF86AudioMute>", spawn "amixer -c 0 -q set Master toggle")
           , ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 -q set Master 5+")
           , ("<XF86AudioLowerVolume>", spawn "amixer -c 0 -q set Master 5-")
           , ("M-S-l", spawn "slock && xset dpms force off") -- lock workstation and turn off display
           , ("M-x", runOrRaisePrompt defaultXPConfig) -- Run or Raise
           , ("<XF86AudioNext>", spawn "ncmpcpp next")
           , ("<XF86AudioPrev>", spawn "ncmpcpp prev")
           , ("<XF86AudioPlay>", spawn "ncmpcpp toggle")
           , ("<XF86AudioStop>", spawn "ncmpcpp stop")
           , ("M-S-p", spawn "synapse") 
           ]

  mySubmap :: [((ButtonMask, KeySym), X ())]
  mySubmap = [ ((mod4Mask, xK_n), submap . M.fromList $
                   [ ((0, xK_n),     spawn "noted_helper -n")
                   , ((0, xK_d),     spawn "noted_helper -d")
                   , ((0, xK_l),     spawn "notedpp -l | dmenu -l 10")
                   ])
             ]

import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.MultiToggle as Multi
import XMonad.Layout.MultiToggle.Instances as MultiI

main :: IO ()

myLayout = Multi.mkToggle1 MultiI.NBFULL $ (myTall ||| myFull)
  where myTall = spacingWithEdge 10 $ Tall 1 (3/100) (1/2)
        myFull = Full

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myWorkspaces = map show [1..9]

myBar = "xmobar"
myPP = xmobarPP {
  ppCurrent = xmobarColor "#FFFFFF" "#444444" . wrap "[" "]"
  , ppHiddenNoWindows = xmobarColor "#888888" ""
  }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keys = 
  [
    ("M-<Space>", spawn "dmenu_run -b -fn 'Fira Code-11' -nb black -sb '#555555' -nf white")
  , ("M-<Return>", spawn "kitty")
  , ("M-e", spawn "emacs")
  , ("M-f", sendMessage $ Multi.Toggle MultiI.NBFULL)
  , ("M-d", spawn "~/.config/Ankama/Retro/dofus1electron")
  ]
  
myConfig = def
  {
    borderWidth = 2,
    focusFollowsMouse = False,
    focusedBorderColor = "white",
    normalBorderColor = "#333333",
    modMask = mod4Mask,
    layoutHook = myLayout,
    workspaces = myWorkspaces
  }
  `additionalKeysP` Main.keys


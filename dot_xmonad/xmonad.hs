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
import XMonad.Actions.WindowBringer

main :: IO ()

myLayout = Multi.mkToggle1 MultiI.NBFULL $ (myTall ||| myFull)
  where myTall = spacingWithEdge 3 $ Tall 1 (3/100) (1/2)
        myFull = Full

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig


myWorkspaces = (map pad) . (map show) $ [1..9]

myppTitle :: String -> String
myppTitle s = ""

myppLayout :: String -> String
myppLayout s = ""

underlineWorkspace s = "<box type=Bottom width=4>" ++ s ++ "</box>"

myBar = "xmobar"
myPP = xmobarPP {
  ppCurrent = underlineWorkspace
  , ppHidden = xmobarColor "#FFFFFF" "#000000"
  , ppHiddenNoWindows = xmobarColor "#888888" "#000000"
  , ppTitle = myppTitle
  , ppLayout = myppLayout
  , ppWsSep = "|"
  }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keys = 
  [
    ("M-<Space>", spawn "dmenu_run -b -fn 'Fira Code Medium-11' -nb black -sb '#555555' -nf white")
  , ("M-<Return>", spawn "kitty")
  , ("M-e", spawn "emacs")
  , ("M-f", sendMessage $ Multi.Toggle MultiI.NBFULL)
  , ("M-d", spawn "~/.config/Ankama/Retro/dofus1electron")
  , ("M-w", gotoMenu)
  , ("<XF86AudioLowerVolume>", spawn "amixer -c 0 set Master 5-")
  , ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 set Master 5+")
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


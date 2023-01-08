import Text.Printf
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
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile

import XMonad.Actions.MouseResize
import XMonad.Layout.WindowArranger

import XMonad.Actions.WindowBringer

import XMonad.Hooks.Place

main :: IO ()

myLayout = Multi.mkToggle1 MultiI.NBFULL $ (myTall ||| myFull)
  where myTall = spacingWithEdge 3 $ ResizableTall 1 (3/100) (1/2) []
        myFull = noBorders Full

-- myLayout = ResizableTall 1 (3/100) (1/2) [] ||| Full ||| mouseResizableTile

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig



myppTitle :: String -> String
myppTitle s = ""

myppLayout :: String -> String
myppLayout s = ""

underlineWorkspace :: String -> String
underlineWorkspace s = printf "<box type=Bottom width=4>%s</box>" s

currentWorkspaceColor = "#fb2e01"
colorWorkspace :: String -> String -> String
colorWorkspace color s = printf "<fc=%s>%s</fc>" color s

fontWorkspace :: Int -> String -> String
fontWorkspace n s = printf "<fn=%d>%s</fn>" n s

myWorkspaces = (map pad) . (map show) $ [1..9]

myBar = "xmobar"
myPP = xmobarPP {
  ppCurrent = (fontWorkspace 1) . (colorWorkspace currentWorkspaceColor)
  -- ppCurrent = (fontWorkspace 1) . (colorWorkspace currentWorkspaceColor)
  , ppHidden = (fontWorkspace 1) . xmobarColor "#FFFFFF" "#131316"
  , ppHiddenNoWindows = (fontWorkspace 1) . xmobarColor "#888888" "#131316"
  , ppTitle = myppTitle
  -- , ppLayout = myppLayout
  , ppWsSep = "|"
  }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

dmenu_config = "-b -fn 'Fira Code Medium-11' -nb black -sb '#555555' -nf white"

keys_to_add = 
  [
    ("M-<Space>", spawn $ "dmenu_run " ++ dmenu_config)
  , ("M-d", spawn $ "dmenu_run " ++ dmenu_config)
  , ("M-<Return>", spawn "kitty -1")
  -- , ("M-e", spawn "emacsclient -c")
  , ("M-e", spawn "emacs")
  , ("M-f", sendMessage $ Multi.Toggle MultiI.NBFULL)
  , ("M-D", spawn "~/.config/Ankama/Retro/dofus1electron")
  , ("M-w", gotoMenu)
  , ("<XF86AudioLowerVolume>", spawn "amixer -c 0 set Master 5-")
  , ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 set Master 5+")
  , ("M-s", spawn $ "~/.config/scripts/dmenu_ssh.sh " ++ dmenu_config)
  ]

keys_to_remove = 
  [
    -- (mod4Mask, button1)
  -- , (mod4Mask, button2)
  -- , (mod4Mask, button3)
  ]

myManageHook = composeAll 
  [
    className =? "Xmessage" --> doFloat
  ]

myPlacement = withGaps (16,0,16,0) (smart (0.5,0.5))
myManageHookPlace = placeHook myPlacement 
  
myConfig = def
  {
    borderWidth = 2,
    focusFollowsMouse = False,
    focusedBorderColor = "white",
    normalBorderColor = "#333333",
    modMask = mod4Mask,
    layoutHook = myLayout,
    workspaces = myWorkspaces,
    manageHook = myManageHookPlace <> myManageHook

  }
  `additionalKeysP` Main.keys_to_add
  `removeMouseBindings` Main.keys_to_remove


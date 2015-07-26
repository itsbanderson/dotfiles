import Data.Ratio ((%))
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

tall = spacing 5 $ Tall 1 (3/100) (2/3) -- Tall layout
myLayout = avoidStruts $ smartBorders (tall ||| Mirror tall ||| Full)
myWorkspaces = ["1", "2", "3", "4", "5"]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/ben/.xmobarrc"
  xmonad $ defaultConfig
    {manageHook = manageDocks
        <+> (className =? "mpv" --> doFullFloat)
        <+> manageHook defaultConfig
    , terminal = "urxvt"
    , focusFollowsMouse = False
    , layoutHook = myLayout
    , workspaces = myWorkspaces
    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                }
    , modMask = mod4Mask -- Rebind Mod to the Windows/Command key
    -- , normalBorderColor = ""
    , focusedBorderColor = "#FFFFFF"
    , borderWidth = 0
    }
    `additionalKeys`
    [
        ((mod4Mask .|. shiftMask, xK_z), spawn "slock")
        , ((mod4Mask .|. shiftMask, xK_P), spawn "sleep 0.2; scrot -s")
        , ((mod4Mask .|. shiftMask, xK_o), spawn "scrot")
        -- Make the mac keys work (also requires xorgxbacklight be installed)
        , ((mod4Mask, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
        , ((mod4Mask, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
        , ((mod4Mask, xF86XK_AudioLowerVolume), spawn "amixer set Master 3dB-")
        , ((mod4Mask, xF86XK_AudioRaiseVolume), spawn "amixer set Master 3dB+")
        , ((mod4Mask, xF86XK_AudioMute), spawn "amixer set Master toggle")
        -- Center the current window
        , ((mod4Mask, xK_a), withFocused (keysMoveWindowTo (640,450) (1%2,1%2)))
        -- These resizing things are broken, but I'm not sure why
        , ((mod4Mask .|. mod2Mask, xK_h), withFocused (keysResizeWindow (-10, 0) (1,1)))
        , ((mod4Mask, xK_s), withFocused (keysResizeWindow (10,10) (1,1)))
        , ((mod4Mask .|. shiftMask, xK_d), withFocused (keysAbsResizeWindow (-10,-10) (1024,752)))
        , ((mod4Mask .|. shiftMask, xK_s), withFocused (keysAbsResizeWindow (10,10) (1024,752)))
    ]

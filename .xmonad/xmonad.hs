import Data.Ratio ((%))
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops as EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

myLayout = avoidStruts $ smartBorders (tall ||| Full)
    where tall = spacing 5 $ Tall 1 (3/100) (2/3) -- Tall layout
myWorkspaces = ["1", "2", "3", "4", "5"]
-- Force these apps to do fullscreen
myFloats = [
    "mpv"
    ]
-- mod1Mask = left Alt, mod4Make = left windows/Command key (good for Mac)
mainMod = mod1Mask

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/ben/.xmobarrc"
  --ewmh should make it better about respecting full screen, etc.
  xmonad $ ewmh defaultConfig
    {
        manageHook = manageDocks
            <+> composeAll [className =? c --> doFullFloat | c <- myFloats]
            <+> (isFullscreen --> doFullFloat)
            <+> manageHook defaultConfig
        , handleEventHook = handleEventHook defaultConfig
            <+> EwmhDesktops.fullscreenEventHook
        , terminal = "urxvt"
        , focusFollowsMouse = False
        , layoutHook = myLayout
        , workspaces = myWorkspaces
        , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" . shorten 50
                    }
        , modMask = mainMod 
    }
    `additionalKeys`
    [
        ((mainMod .|. shiftMask, xK_z), spawn "slock")
        , ((mainMod .|. shiftMask, xK_P), spawn "sleep 0.2; scrot -s")
        , ((mainMod .|. shiftMask, xK_o), spawn "scrot")
        -- Toggle gaps
        ,((mainMod, xK_b), sendMessage ToggleStruts)
        ,((mainMod, xK_g), goToSelected defaultGSConfig)
    ]

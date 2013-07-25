import XMonad
import XMonad.Util.EZConfig
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Config.Xfce
import XMonad.Layout.ResizableTile
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W


main = xmonad $ xfceConfig
        { layoutHook         = smartBorders $ myLayout
        , workspaces         = myWorkspaces
        , modMask            = mod4Mask
        , terminal           = "pantheon-terminal"
        , focusedBorderColor = "Deep Pink"
        , borderWidth        = 2
        , manageHook         = myManageHook <+> manageHook xfceConfig
        }
        `additionalKeysP` myKeysP
        `additionalKeys` myKeys

myLayout = maximize $ avoidStruts (
    tall |||
    Full |||
    TwoPane (3/100) (1/2) |||
    Mirror tall
    ) |||
    noBorders (fullscreenFull Full)
    where
        tall = ResizableTall 1 (3/100) (5/10) []

myManageHook::ManageHook
myManageHook = composeAll . concat $
    [ [ className =? c --> doFloat       | c <- myFloats ]
    , [ title     =? t --> doFloat       | t <- myTitleFloats ]
    , [ className =? c --> doIgnore      | c <- myIgnores ]
    , [ title     =? t --> doIgnore      | t <- myTitleIgnore ]
    , [ isFullscreen --> doFullFloat ]
    ]
    where myFloats      = [ "Gimp", "Vncviewer", "vlc" ]
          myTitleFloats = [ "wicd-gtk",  "kupfer" , "Application Finder" ]
          myIgnores     = [ "xfce4-notifyd", "xfce4-power-manager"]
          myTitleIgnore = []

myWorkspaces :: [String]
myWorkspaces =
    [ "web"  , "irc"
    , "dev"  , "x"
    , "music"
    ]

myKeysP = [
          ("M-<shift>-<enter>", spawn "pantheon-terminal")
        , ("M-p", spawn "exec kupfer")
        , ("M-q", restart "xmonad" True)
        , ("M--", sendMessage MirrorShrink)
        , ("M-=", sendMessage MirrorExpand)
        ]
modm = mod4Mask
myKeys = [
          ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((modm .|. shiftMask,       xK_Right), shiftToNext >> nextWS)
        , ((modm .|. shiftMask,       xK_Left),  shiftToPrev >> prevWS)
        , ((modm,                     xK_Right), nextWS)
        , ((modm,                     xK_Left),  prevWS)
        , ((modm,                     xK_z),     toggleWS)
        , ((modm,                     xK_m),     withFocused (sendMessage . maximizeRestore))
        , ((modm .|. controlMask,       xK_k),     kill)
        , ((controlMask .|. mod1Mask, xK_Delete), spawn "slimlock")
        -- XF86AudioMute
        , ((0 , 0x1008ff12), spawn "amixer -q set Master toggle")
        -- XF86AudioLowerVolume
        , ((0 , 0x1008ff11), spawn "amixer -q set Master 3- unmute")
        -- XF86AudioRaiseVolume
        , ((0 , 0x1008ff13), spawn "amixer -q set Master 3+ unmute")
        -- XF86AudioNext
        , ((0,  0x1008ff17), spawn "mpris-remote next")
        -- XF86AudioPrevious
        , ((0,  0x1008ff16), spawn "mpris-remote previous")
        -- XF86AudioPlay
        , ((0,  0x1008ff14), spawn "mpris-remote toggle")
        -- XF86AudioStop
        , ((0,  0x1008ff15), spawn "mpris-remote stop")
        ]


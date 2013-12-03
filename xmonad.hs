-- vim :fdm=marker sw=4 sts=4 ts=4 et ai:
 
-- Imports {{{
import Data.Ratio
import qualified Data.List as DL

import XMonad
import XMonad.Layout
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.ThreeColumns
import XMonad.Layout.IM
import XMonad.Layout.Fullscreen as FS
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Gaps
import XMonad.Layout.Tabbed
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.TwoPane

import XMonad.Util.WindowProperties


import Control.Monad

import XMonad.Hooks.DynamicLog   (PP(..), dynamicLogWithPP, wrap, defaultPP)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ICCCMFocus
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Hooks.SetWMName

import System.IO.UTF8

import XMonad.Prompt
import XMonad.Prompt.Shell

import qualified XMonad.StackSet as W

import qualified Data.Map as M

import qualified System.Directory as SD

import System.IO.Unsafe (unsafePerformIO)
 
-- }}}
 
-- Control Center {{{
-- Colour scheme {{{
myNormalBGColor     = "#2e3436"
myFocusedBGColor    = "#414141"
myNormalFGColor     = "#babdb6"
myFocusedFGColor    = "#73d216"
myUrgentFGColor     = "#f57900"
myUrgentBGColor     = myNormalBGColor
mySeperatorColor    = "#2e3436"
-- }}}
-- Icon packs can be found here:
-- http://robm.selfip.net/wiki.sh/-main/DzenIconPacks
myBitmapsDir        = "/home/xilon/.share/icons/dzen"
myFont              = "-*-terminus-medium-r-*-*-12-*-*-*-*-*-iso10646-1"
-- }}}
 
-- Workspaces {{{
myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "general", "code", "internet", "pidgin", "skype", "gimp" ] ++ map show [7..8 :: Int] ++ [ "mp3" ]
-- }}}
 
statusBarCmd= "dzen2 -p -h 16 -ta l -bg '" ++ myNormalBGColor ++ "' -fg '" ++ myNormalFGColor ++ "' -w 400 -sa c -fn '" ++ myFont ++ "'"
 
-- Main {{{

scripts_dir = (unsafePerformIO $ SD.getHomeDirectory) ++ "/xmonad-bin/"

skype_username = "username"

main = do
    statusBarPipe <- spawnPipe statusBarCmd
    xmonad . ewmh $ withUrgencyHook NoUrgencyHook $ myConfig statusBarPipe

myConfig pipe = defaultConfig {
        modMask = mod4Mask,
        borderWidth = 1,
        terminal = scripts_dir ++ "rxvt",
        normalBorderColor = myNormalBGColor,
        focusedBorderColor = myFocusedFGColor,
        startupHook = setWMName "LG3D",
        handleEventHook = FS.fullscreenEventHook,
        manageHook = manageHook defaultConfig <+> myManageHook <+> FS.fullscreenManageHook,
        layoutHook = fullscreenFull . gaps [(U,16)] $ 
                        onWorkspace "pidgin" chatLayout $ 
                        onWorkspace "skype" skypeLayout $ 
                        onWorkspace "gimp" gimpLayout globalLayout,
        workspaces = myWorkspaces,
        focusFollowsMouse=False,
        logHook = (dynamicLogWithPP $ myPP pipe) >> takeTopFocus
        --logHook = dynamicLogWithPP $ myPP pipe
    }
    `additionalKeysP` [
        ("M-f", spawn "firefox"),
        ("M-c", spawn "chromium"),
        ("<F12>", spawn (scripts_dir ++ "rxvt")),
        ("M-<F12>", spawn (scripts_dir ++ "rxvt-start")),
        ("<Print>", spawn "xwd -out screenshot.xwd"),
        ("C-<Print>", spawn "xwd -root -out screenshot.xwd"),
--        ("M-e", spawn "eclipse"),
        ("M-p", shellPrompt (greenXPConfig {font = myFont})),
        ("M1-<F4>", kill),
        ("<XF86Calculator>", spawn (scripts_dir ++ "toggle-touchpad")),
        ("M-<Left>", prevWS),
        ("M-<Right>", nextWS),
        ("M-S-l", spawn ("vlock -n -a"))
    ]
    `additionalKeys` [
        ((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces numPadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]

    ]
    `additionalKeys` [
        ((mod1Mask, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
    ]

numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
    , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
    , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
    , xK_KP_Insert]


globalLayout = fullscreenFull $ ResizableTall 1 (3/100) (3/4) [] ||| simpleTabbed
chatLayout = reflectHoriz $ IM (1%6) (Title "Buddy List")
skypeLayout = reflectHoriz $ IM (1%6) (Title ( skype_username ++ "- Skype™"))
gimpLayout = withIM (1%6) (Role "gimp-toolbox") $ reflectHoriz $ withIM (1%6) (Role "gimp-dock") Full

--    skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))
 
-- Window rules (floating, tagging, etc) {{{
myManageHook = composeAll [
        className   =? "Firefox"            --> doF(W.shift "internet"),
        className   =? "Chromium"           --> doF(W.shift "internet"),
        className   =? "Pidgin"             --> doF(W.shift "pidgin"),
        className   =? "Skype"              --> doF(W.shift "skype"),
        className   =? "Eclipse"            --> doF(W.shift "code"),
        matchTitle  "IntelliJ IDEA"         --> doF(W.shift "code"),
        className   =? "Gimp"               --> doF(W.shift "gimp"),
        className   =? "stalonetray"        --> doIgnore,
        className   =? "trayer"             --> doIgnore
    ]

matchTitle ::  [Char] -> Query Bool
matchTitle s = DL.isInfixOf s `fmap` title
-- }}}
 
-- Dzen Pretty Printer {{{
-- Stolen from Rob [1] and modified
-- [1] http://haskell.org/haskellwiki/Xmonad/Config_archive/Robert_Manea%27s_xmonad.hs
myPP handle = defaultPP {
        ppCurrent = wrap ("^fg(" ++ myFocusedFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppUrgent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myUrgentBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppSep     = "^fg(" ++ mySeperatorColor ++ ")^r(3x3)^fg()",
        ppTitle   = wrap ("^fg(" ++ myFocusedFGColor ++ ")") "^fg()" ,
        ppOutput  = hPutStrLn handle
}

-- }}} 

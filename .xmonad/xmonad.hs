{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Default
import           Data.List (isSuffixOf)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Ratio ((%))
import           System.Exit
import           System.IO
import           XMonad.Layout.Accordion
import           XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import           XMonad
import           XMonad.Actions.CycleWindows
import           XMonad.Actions.CycleWS
import           XMonad.Actions.GridSelect
import           XMonad.Actions.OnScreen
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Place
import           XMonad.Hooks.CurrentWorkspaceOnTop (currentWorkspaceOnTop)
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.NoBorders
import           XMonad.Layout.SimpleFloat (simpleFloat)
import           XMonad.Prompt.Window
import           XMonad.Util.EZConfig
import           XMonad.Util.Run (spawnPipe)
import           XMonad.Util.WorkspaceCompare
import System.Taffybar.Hooks.PagerHints (pagerHints)
import           Graphics.X11.ExtraTypes.XF86

import           Xkb

primaryWorkspaces :: [(String, String)]
primaryWorkspaces =
  [ ("term", "[")
  , ("misc", "&")
  , ("emacs", "{")
  , ("web", "}")
  , ("msg", "(")
  , ("misc2", "=")
  ]

secondaryWorkspaces :: [(String, String)]
secondaryWorkspaces =
  [ ("secondary", "*")
  , ("coins", ")")
  , ("passwd", "+")
  , ("secondary2", "!")
  , ("secondary3", "]")
  , ("secondary4", "#")
  ]

myWorkspaces :: [(String, String)]
myWorkspaces = (p:s:ps) ++ ss
  where p:ps = primaryWorkspaces
        s:ss = secondaryWorkspaces

maxNameLength :: Int
maxNameLength = maximum $ map (length . fst) myWorkspaces

padWorkspaceName :: String -> String
padWorkspaceName x = " " ++ x ++ take (1 + maxNameLength - length x) (repeat ' ')

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Sshmenu"        --> doFloat
    , isFullscreen                  --> doFullFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Xfce4-notifyd"  --> doIgnore
    , className =? "Wine"           --> doFloat
    , className =? "Gnome-osd-server"  --> doFloat
    , className =? "Chromium"       --> doF (W.shift "web")
    , className =? "Conkeror"       --> doF (W.shift "web")
    , className =? "Thunderbird"    --> doF (W.shift "misc2")
    , className =? "Icedove"        --> doF (W.shift "misc2")
    , className =? "Skype"          --> doF (W.shift "msg")
    , className =? "Viber"          --> doF (W.shift "msg")
    , className =? "quassel"        --> doF (W.shift "secondary4")
    , className =? "Workrave"       --> doFloat <+> doF (W.shift "secondary4")
    , fmap (isSuffixOf "KeePass") title --> doF (W.shift "passwd")
    , title     =? "Simple Demo with Shaders" --> doFloat
    ]

myLayout = smartBorders Full ||| Mirror tiled ||| tiled
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 5/100

myLayoutHook =
  xkbLayout $
  avoidStruts $
  onWorkspace "coins" Grid $
  onWorkspace "passwd" (noBorders Grid) $
  onWorkspace "secondary" simpleFloat $
  onWorkspace "msg" (withIM (1%5) (Title "binarin - Skype™") Grid) $
  myLayout

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_slash) , substringSearch myNavigation)
         ,((0,xK_Left)  , move (-1,0)  >> myNavigation)
         ,((0,xK_d)     , move (-1,0)  >> myNavigation)
         ,((0,xK_Right) , move (1,0)   >> myNavigation)
         ,((0,xK_n)     , move (1,0)   >> myNavigation)
         ,((0,xK_Down)  , move (0,1)   >> myNavigation)
         ,((0,xK_h)     , move (0,1)   >> myNavigation)
         ,((0,xK_Up)    , move (0,-1)  >> myNavigation)
         ,((0,xK_t)     , move (0,-1)  >> myNavigation)
         ,((0,xK_f)     , move (-1,-1) >> myNavigation)
         ,((0,xK_c)     , move (1,-1)  >> myNavigation)
         ,((0,xK_b)     , move (-1,1)  >> myNavigation)
         ,((0,xK_m)     , move (1,-1)  >> myNavigation)
         ,((0,xK_space) , setPos (0,0) >> myNavigation)
         ]
       -- The navigation handler ignores unknown key symbols
       navDefaultHandler = const myNavigation

gsconfig1 :: HasColorizer a => GSConfig a
gsconfig1 = def
    { gs_navigate = myNavigation }

main :: IO ()
main = do
  xmonad $ myConfig

myManageFloats :: ManageHook
myManageFloats = placeHook $ inBounds $ withGaps (16,0,16,0) (smart (0.5,0.5))

myConfig = ewmh $ pagerHints $ def {
  modMask = mod4Mask
  , workspaces = map fst myWorkspaces
  , terminal           = "urxvt"
  , borderWidth        = 3
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#cd8b00"
  , manageHook = myManageFloats <+> manageDocks <+> myManageHook <+> manageHook def
  , handleEventHook = mconcat $
                      [ docksEventHook
                      , fullscreenEventHook
                      ]
  , layoutHook = myLayoutHook
  , logHook = currentWorkspaceOnTop
  }
        `additionalKeysP`
        ([ ("M-y", spawn "urxvt")
        -- , ("M-p", withFocused (\windowId -> do { floats <- gets (W.floating . windowset); if windowId `M.member` floats then withFocused $ windows . W.sink else float windowId }))
        , ("M-j", windowPromptGoto def)
        , ("M-;", spawn "sshmenu")
        , ("M-l", spawn "exe=$(yeganesh -x) && exec $exe")
        , ("M-S-l", spawn "gmrun")
        , ("M-<Print>", spawn "shutter -w")
        , ("M-q", spawn "xmonad --recompile && xmonad --restart")
        , ("M-S-q", io (exitWith ExitSuccess))
        , ("M-b", refresh)
        , ("M-h", windows W.focusDown)
        , ("M-t", windows W.focusUp)
        , ("M-m", windows W.focusMaster)
        , ("M-S-h", windows W.swapDown)
        , ("M-S-t", windows W.swapUp)
        , ("M-d", sendMessage Shrink)
        , ("M-n", sendMessage Expand)
        , ("M-w", sendMessage (IncMasterN 1))
        , ("M-v", sendMessage (IncMasterN (-1)))
        , ("M-'", goToSelected gsconfig1)
        , ("M-s", withFocused $ windows . W.sink)
        , ("M-<Right>", nextScreen)
        , ("M-<Left>", prevScreen)
        , ("M-<Up>", gridselectWorkspace gsconfig1 (\ws -> W.shift ws))
        , ("M-S-,", screenWorkspace 0 >>= flip whenJust (windows . W.shift))
        , ("M-S-.", screenWorkspace 1 >>= flip whenJust (windows . W.shift))
        , ("M-S-p", screenWorkspace 2 >>= flip whenJust (windows . W.shift))
        , ("M-k", kill)
        , ("M-<Backspace>", cycleRecentWindows [xK_Super_L, xK_Super_R] xK_BackSpace xK_Delete)
        , ("C-\\", sendMessage (XkbToggle Nothing))
        -- , ("M-S-<Backspace>", removeWorkspace)
        -- , ("M-S-j", selectWorkspace defaultXPConfig)
        -- , ("M-a", withWorkspace defaultXPConfig (windows . W.shift))
        -- , ("M-r", addWorkspacePrompt defaultXPConfig)
        -- , ("M-S-a", withWorkspace defaultXPConfig (windows . copy))
        -- , ("M-S-r", renameWorkspace defaultXPConfig)
        ]
         ++ [ ("M-" ++ key, windows $ viewPrimary name)
            | (name, key) <- primaryWorkspaces ]
         ++ [ ("M-" ++ key, windows $ viewSecondary name)
            | (name, key) <- secondaryWorkspaces ]
        )
        `additionalKeys`
        [ ((0, xF86XK_Mail), return ())
        , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +5")
        , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -5")
        , ((0, xF86XK_TouchpadToggle), spawn "toggle-touchpad")
        , ((0, xF86XK_AudioLowerVolume), spawn "volume.sh down")
        , ((0, xF86XK_AudioRaiseVolume), spawn "volume.sh up")
        , ((0, xF86XK_AudioMute), spawn "volume.sh mute")
        , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
        , ((0, xF86XK_AudioNext), spawn "playerctl next")
        , ((0, xF86XK_AudioPrev), spawn "playerctl previous")
        ]

viewPrimary, viewSecondary :: WorkspaceId -> WindowSet -> WindowSet
viewPrimary i ss@(W.StackSet {W.visible = []}) = W.view i ss
viewPrimary i ss = greedyViewOnScreen 0 i ss

viewSecondary i ss@(W.StackSet {W.visible = []}) = W.view i ss
viewSecondary i ss = greedyViewOnScreen 1 i ss

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when, join)
import Data.Maybe (maybeToList)
-- import           Data.Function ((&))
import           Data.Default
import qualified Data.Map as M
-- import           Data.Monoid
import           Data.Ratio ((%))
import           System.Exit

import           System.Taffybar.Hooks.PagerHints (pagerHints)
import           XMonad
import           XMonad.Hooks.CurrentWorkspaceOnTop (currentWorkspaceOnTop)
import           XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks (docks, avoidStruts)
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen, doCenterFloat)
import           XMonad.Hooks.Place (smart, withGaps, inBounds, placeHook)
import           XMonad.Hooks.UrgencyHook (withUrgencyHookC, NoUrgencyHook(NoUrgencyHook), focusUrgent, urgencyConfig)
import qualified XMonad.Hooks.UrgencyHook as Urgency
import           XMonad.Layout.Fullscreen (fullscreenSupport)
import           XMonad.Layout.SimpleFloat (simpleFloat)
import qualified XMonad.StackSet as W

import           XMonad.Actions.CycleWindows
import           XMonad.Actions.CycleWS
import           XMonad.Actions.GridSelect
import           XMonad.Actions.OnScreen
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.NoBorders
import           XMonad.Util.EZConfig
import           Graphics.X11.ExtraTypes.XF86

import           Xkb

primaryWorkspaces :: [(String, String)]
primaryWorkspaces =
  [ ("term", "[")
  , ("misc", "&")
  , ("emacs", "{")
  , ("web", "}")
  , ("msg", "(")
  , ("jabber", "=")
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

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Sshmenu"        --> doFloat
    , isFullscreen                  --> doFullFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Xfce4-notifyd"  --> doIgnore
    , className =? "Wine"           --> doFloat
    , className =? "Skype"          --> doF (W.shift "msg")
    , className =? "Viber"          --> doF (W.shift "msg")
    , title     =? "FAST_CHOICE"    --> doCenterFloat
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

configModifiers =
      withUrgencyHookC NoUrgencyHook urgencyConfig {Urgency.suppressWhen = Urgency.Never}
    . ewmh
    . pagerHints
    . fullscreenSupport
    . docks

myConfig =  configModifiers def
  { modMask = mod4Mask
  , workspaces = map fst myWorkspaces
  , terminal           = "urxvt"
  , borderWidth        = 3
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#cd8b00"
  , manageHook = myManageFloats <+> myManageHook <+> manageHook def
  , handleEventHook = mconcat $
                      [ handleEventHook def
                      , fullscreenEventHook
                      ]
  , layoutHook = myLayoutHook
  , logHook = currentWorkspaceOnTop
  , startupHook = startupHook def >> addEWMHFullscreen
  }
        `additionalKeysP`
        ([ ("M-y", spawn "urxvt")
         , ("M-;", spawn "sshmenu")
         , ("M-l", spawn "exe=$(yeganesh -x) && exec $exe")
         , ("M-S-l", spawn "gmrun")
         , ("M-<Print>", spawn "shutter -w")
         , ("M-q", spawn "xmonad --recompile && xmonad --restart")
         , ("M-S-q", io (exitWith ExitSuccess))
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
         , ("M-g", focusUrgent)
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
viewSecondary i ss@(W.StackSet {W.visible = (_:_:[]), W.current = W.Screen {W.screen = cur}}) =
  case cur of
    0 -> greedyViewOnScreen 1 i ss
    _ -> greedyViewOnScreen cur i ss
viewSecondary i ss = greedyViewOnScreen 1 i ss

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
        sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
        when (fromIntegral x `notElem` sup) $
          changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.Exit
import System.IO
import Data.Monoid
import Data.Default
import Data.List (isSuffixOf)

import XMonad
-- import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
-- import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.OnScreen

-- import XMonad.Config.Gnome

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
-- import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.CurrentWorkspaceOnTop (currentWorkspaceOnTop)

import XMonad.Layout.Grid
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat(simpleFloat)

-- import XMonad.Prompt
import XMonad.Prompt.Window

import XMonad.Util.EZConfig
-- import XMonad.Util.Loggers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare

-- import qualified DBus as D
-- import qualified DBus.Client as D
-- import qualified Codec.Binary.UTF8.String as UTF8
import Graphics.X11.ExtraTypes.XF86

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
myWorkspaces = primaryWorkspaces ++ secondaryWorkspaces

maxNameLength :: Int
maxNameLength = maximum $ map (length . fst) myWorkspaces

padWorkspaceName :: String -> String
padWorkspaceName x = " " ++ x ++ take (1 + maxNameLength - length x) (repeat ' ')

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Sshmenu"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Xfce4-notifyd"  --> doIgnore
    , className =? "Gnome-osd-server"  --> doFloat
--    , className =? "KeePass2"       --> doF (W.shift "passwd")
    , className =? "Chromium"       --> doF (W.shift "web")
    , className =? "Thunderbird"    --> doF (W.shift "misc2")
    , className =? "Icedove"        --> doF (W.shift "misc2")
    , className =? "Skype"          --> doF (W.shift "msg")
    , className =? "Workrave"       --> doFloat <+> doF (W.shift "secondary4")
    , fmap (isSuffixOf "KeePass") title --> doF (W.shift "passwd")
    , title     =? "Simple Demo with Shaders" --> doFloat
    ]

myLayout = smartBorders Full ||| (mouseResizableTile) ||| (mouseResizableTile { isMirrored = True })

myLayoutHook =
  avoidStruts $
  onWorkspace "coins" Grid $
  onWorkspace "passwd" (noBorders Grid) $
  onWorkspace "secondary" simpleFloat $
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
  xmobarPipe <- spawnPipe "xmobar"
  xmonad $ myConfig xmobarPipe

xmobarPrettyPrinter :: Handle -> PP
xmobarPrettyPrinter xmobarPipe = def
  { ppTitle    = pad
  , ppOutput   = hPutStrLn xmobarPipe
  , ppCurrent  = xmobarColor "#2b4f98" "green" . padWorkspaceName
  , ppVisible  = xmobarColor "grey" "#2b4f98" . padWorkspaceName
  , ppHidden   = const ""
  , ppHiddenNoWindows = const ""
  , ppUrgent   = xmobarColor "red" "yellow" . pad
  , ppWsSep    = ""
  , ppSep      = ""
  , ppLayout   = xmobarColor "yellow" "#2b4f98" .
                 (\x ->
                   pad $ case x of
                          "Full" -> "FL"
                          "MouseResizableTile" -> "M_"
                          "Mirror MouseResizableTile" -> "MM"
                          "Grid" -> "GR"
                          _ -> x
                 )
  , ppOrder = \(ws:layout:windowTitle:extras) -> layout:ws:windowTitle:extras
  , ppSort = mkWsSort getXineramaPhysicalWsCompare
  }

prettyPrinter :: Handle -> PP
prettyPrinter = xmobarPrettyPrinter

myManageFloats :: ManageHook
myManageFloats = placeHook $ inBounds $ withGaps (16,0,16,0) (smart (0.5,0.5))

myConfig logHandle = ewmh def {
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
  , logHook = currentWorkspaceOnTop <+> dynamicLogWithPP (prettyPrinter logHandle)
  }
        `additionalKeysP`
        ([ ("M-y", spawn "rxvt-unicode")
        -- , ("M-p", withFocused (\windowId -> do { floats <- gets (W.floating . windowset); if windowId `M.member` floats then withFocused $ windows . W.sink else float windowId }))
        , ("M-j", windowPromptGoto def)
        , ("M-l", spawn "exe=`~/.cabal/bin/yeganesh -x` && exec $exe")
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
        , ("M-<Right>", nextScreen)
        , ("M-<Left>", prevScreen)
        , ("M-<Up>", gridselectWorkspace gsconfig1 (\ws -> W.shift ws))
        , ("M-S-,", screenWorkspace 0 >>= flip whenJust (windows . W.shift))
        , ("M-S-.", screenWorkspace 1 >>= flip whenJust (windows . W.shift))
        , ("M-S-p", screenWorkspace 2 >>= flip whenJust (windows . W.shift))
        , ("M-k", kill)
        , ("M-<Backspace>", cycleRecentWindows [xK_Super_L, xK_Super_R] xK_BackSpace xK_Delete)
        -- , ("M-S-<Backspace>", removeWorkspace)
        -- , ("M-S-j", selectWorkspace defaultXPConfig)
        -- , ("M-a", withWorkspace defaultXPConfig (windows . W.shift))
        -- , ("M-r", addWorkspacePrompt defaultXPConfig)
        -- , ("M-S-a", withWorkspace defaultXPConfig (windows . copy))
        -- , ("M-S-r", renameWorkspace defaultXPConfig)
        ]
         ++ [ ("M-" ++ key, windows $ greedyViewOnScreen 0 name)
            | (name, key) <- primaryWorkspaces ]
         ++ [ ("M-" ++ key, windows $ W.greedyView name)
            | (name, key) <- secondaryWorkspaces ]
        )
        `additionalKeys`
        [
          ((0, xF86XK_Mail), return ())
        ]

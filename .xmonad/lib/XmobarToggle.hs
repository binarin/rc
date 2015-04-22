{-# LANGUAGE DeriveDataTypeable #-}

module XmobarToggle (
  unmapDocksEventHook, toggleDocksHook
  ) where

import XMonad
import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.Util.ExtensibleState as XS
import Control.Monad (when)
import Data.Monoid
import XMonad.Hooks.ManageDocks

data DockToggleTime = DTT { lastTime :: Time } deriving (Eq, Show, Typeable)

instance ExtensionClass DockToggleTime where
    initialValue = DTT 0

toggleDocksHook :: Int -> KeySym -> Event -> X All
toggleDocksHook to ks ( KeyEvent { ev_event_display = d
                                 , ev_event_type    = et
                                 , ev_keycode       = ekc
                                 , ev_time          = etime
                                 } ) =
        io (keysymToKeycode d ks) >>= toggleDocks >> return (All True)
    where
    toggleDocks kc
        | ekc == kc && et == keyPress = do
            safeSendSignal ["Reveal 0", "TogglePersistent"]
            XS.put ( DTT etime )
        | ekc == kc && et == keyRelease = do
            gap <- XS.gets ( (-) etime . lastTime )
            safeSendSignal [ "TogglePersistent"
                           , "Hide " ++ show (if gap < 400 then to else 0)
                           ]
        | otherwise = return ()

    safeSendSignal s = catchX (io $ sendSignal s) (return ())
    sendSignal    = withSession . callSignal
    withSession mc = D.connectSession >>= \c -> D.callNoReply c mc >> D.disconnect c
    callSignal :: [String] -> D.MethodCall
    callSignal s = ( D.methodCall
                     ( D.objectPath_    "/org/Xmobar/Control" )
                     ( D.interfaceName_ "org.Xmobar.Control"  )
                     ( D.memberName_    "SendSignal"          )
                   ) { D.methodCallDestination = Just $ D.busName_ "org.Xmobar.Control"
                     , D.methodCallBody        = map D.toVariant s
                     }

toggleDocksHook _ _ _ = return (All True)

unmapDocksEventHook :: Event -> X All
unmapDocksEventHook e = do
    when (et == mapNotify || et == unmapNotify) $
        whenX ((not `fmap` (isClient w)) <&&> runQuery checkDock w) refresh
    return (All True)
    where w  = ev_window e
          et = ev_event_type e

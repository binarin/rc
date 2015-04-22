{-# LINE 1 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface, MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances, PatternGuards #-}
{-# LINE 2 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- GHC 6.10.4 complains about Foreign.C.Types, see Ticket #3419

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.PerWindowKbdLayout
-- Copyright   :  (c) Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A hook that remembers per-window keyboard layouts and switches them
-- on focus changes.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.PerWindowKbdLayout (
                                -- * Usage
                                -- $usage
                                perWindowKbdLayout) where

import Foreign
import Foreign.C.Types (CUChar,CUShort,CUInt(..),CInt(..))

import Control.Monad (when)
import Data.List (find)
import qualified Data.Map as M
import Data.Monoid (All(..))
import Data.Traversable (traverse)

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS


{-# LINE 38 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.PerWindowKbdLayout
--
-- Then edit your @eventHook@ by adding 'perWindowKbdLayout', for example
--
-- > main = xmonad defaultConfig { handleEventHook = perWindowKbdLayout }

data XkbStateRec = XkbStateRec {
    group :: CUChar,
    locked_group :: CUChar,
    base_group :: CUShort,
    latched_group :: CUShort,
    mods :: CUChar,
    base_mods :: CUChar,
    latched_mods :: CUChar,
    locked_mods :: CUChar,
    compat_state :: CUChar,
    grab_mods :: CUChar,
    compat_grab_mods :: CUChar,
    lookup_mods :: CUChar,
    compat_lookup_mods :: CUChar,
    ptr_buttons :: CUShort
}

instance Storable XkbStateRec where
    sizeOf _ = ((18))
{-# LINE 67 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
    alignment _ = alignment (undefined :: CUShort)
    peek ptr = do
        r_group <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 70 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_locked_group <- ((\hsc_ptr -> peekByteOff hsc_ptr 1)) ptr
{-# LINE 71 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_base_group <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) ptr
{-# LINE 72 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_latched_group <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 73 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 6)) ptr
{-# LINE 74 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_base_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 7)) ptr
{-# LINE 75 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_latched_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 76 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_locked_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 9)) ptr
{-# LINE 77 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_compat_state <- ((\hsc_ptr -> peekByteOff hsc_ptr 10)) ptr
{-# LINE 78 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_grab_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 11)) ptr
{-# LINE 79 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_compat_grab_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 80 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_lookup_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 13)) ptr
{-# LINE 81 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_compat_lookup_mods <- ((\hsc_ptr -> peekByteOff hsc_ptr 14)) ptr
{-# LINE 82 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        r_ptr_buttons <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 83 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
        return XkbStateRec {
            group = r_group,
            locked_group = r_locked_group,
            base_group = r_base_group,
            latched_group = r_latched_group,
            mods = r_mods,
            base_mods = r_base_mods,
            latched_mods = r_latched_mods,
            locked_mods = r_locked_mods,
            compat_state = r_compat_state,
            grab_mods = r_grab_mods,
            compat_grab_mods = r_compat_grab_mods,
            lookup_mods = r_lookup_mods,
            compat_lookup_mods = r_compat_lookup_mods,
            ptr_buttons = r_ptr_buttons
        }

foreign import ccall unsafe "X11/XKBlib.h XkbGetState"
    xkbGetState :: Display -> CUInt -> Ptr XkbStateRec -> IO CInt

foreign import ccall unsafe "XkbLockGroup" xkbLockGroup :: Display -> CUInt -> CUInt -> IO ()

type KbdLayout = Int

getKbdLayout :: Display -> IO KbdLayout
getKbdLayout d = alloca $ \stRecPtr -> do
    xkbGetState d (256) stRecPtr
{-# LINE 110 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}
    st <- peek stRecPtr
    return $ fromIntegral (group st)

setKbdLayout :: Display -> KbdLayout -> IO ()
setKbdLayout d l = xkbLockGroup d (256) $ fromIntegral l
{-# LINE 115 "XMonad/Hooks/PerWindowKbdLayout.hsc" #-}

data LayoutStorage = LayoutStorage (Maybe Window) (M.Map Window KbdLayout) deriving (Typeable,Read,Show)
instance ExtensionClass LayoutStorage where initialValue = LayoutStorage Nothing M.empty

perWindowKbdLayout :: Event -> X All
perWindowKbdLayout (DestroyWindowEvent {ev_window = w, ev_event_type = et}) = do
    when (et == destroyNotify) $
        XS.modify $ \(LayoutStorage mpf wtl) -> (LayoutStorage mpf (M.delete w wtl))
    return (All True)
perWindowKbdLayout _ = do
    mst <- gets (W.stack . W.workspace . W.current . windowset)
    traverse update $ W.focus `fmap` mst
    return (All True)

update :: Window -> X()
update foc = withDisplay $ \dpy -> do
    (LayoutStorage mpf wtl) <- XS.get
    curLayout <- io $ getKbdLayout dpy
    case mpf of
        Nothing ->
            XS.put (LayoutStorage (Just foc) (M.insert foc curLayout wtl))
        Just pf -> when (pf /= foc) $ do
            XS.put (LayoutStorage (Just foc) (M.insert pf curLayout wtl))
            io $ whenJust (M.lookup foc wtl) (setKbdLayout dpy)

-- vim:ft=haskell:ts=4:shiftwidth=4:softtabstop=4:expandtab:
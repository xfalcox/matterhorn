{-# LANGUAGE QuasiQuotes #-}

module Events.UrlSelect where

import Prelude ()
import Prelude.Compat

import Brick.Widgets.List
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

onEventUrlSelect :: Vty.Event -> MH ()
onEventUrlSelect e
    | Just kb <- lookupKeybinding e urlSelectKeybindings = kbAction kb
    | otherwise = mhHandleEventLensed csUrlList handleListEvent e

urlSelectKeybindings :: [Keybinding]
urlSelectKeybindings =
    [ [key|Enter Open the selected URL, if any|] $ do
             openSelectedURL
             csMode .= Main

    , [key|Esc Cancel URL selection|] stopUrlSelect
    , [key|q   Cancel URL selection|] stopUrlSelect

    , [key|j   Move cursor down|] $
           mhHandleEventLensed csUrlList handleListEvent (Vty.EvKey Vty.KDown [])

    , [key|k   Move cursor up|] $
           mhHandleEventLensed csUrlList handleListEvent (Vty.EvKey Vty.KUp [])

    ]

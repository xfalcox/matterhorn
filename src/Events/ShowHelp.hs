{-# LANGUAGE QuasiQuotes #-}

module Events.ShowHelp where

import Prelude ()
import Prelude.Compat

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

onEventShowHelp :: Vty.Event -> MH ()
onEventShowHelp e | Just kb <- lookupKeybinding e helpKeybindings =
  kbAction kb
onEventShowHelp (Vty.EvKey _ _) = do
  csMode .= Main
onEventShowHelp _ = return ()

helpKeybindings :: [Keybinding]
helpKeybindings =
    [ [key|Up       Scroll up|]   $ vps (-1)
    , [key|Down     Scroll down|] $ vps 1
    , [key|PageUp   Page up|]     $ vps (-1 * pageAmount)
    , [key|PageDown Page down|]   $ vps pageAmount
    , [key|Space    Page down|]   $ vps pageAmount
    , [key|Esc      Return to the main interface|] $ csMode .= Main
    ]
    where vps = mh . vScrollBy (viewportScroll HelpViewport)

{-# LANGUAGE QuasiQuotes #-}

module Events.MessageSelect where

import Prelude ()
import Prelude.Compat

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

messagesPerPageOperation :: Int
messagesPerPageOperation = 10

onEventMessageSelect :: Vty.Event -> MH ()
onEventMessageSelect e | Just kb <- lookupKeybinding e messageSelectKeybindings =
  kbAction kb
onEventMessageSelect _ = return ()

onEventMessageSelectDeleteConfirm :: Vty.Event -> MH ()
onEventMessageSelectDeleteConfirm (Vty.EvKey (Vty.KChar 'y') []) = do
    deleteSelectedMessage
    csMode .= Main
onEventMessageSelectDeleteConfirm _ =
    csMode .= Main

messageSelectKeybindings :: [Keybinding]
messageSelectKeybindings =
    [ [key|Esc  Cancel message selection|]    (csMode .= Main)
    , [key|C-c  Cancel message selection|]    (csMode .= Main)
    , [key|k    Select the previous message|] messageSelectUp
    , [key|Up   Select the previous message|] messageSelectUp
    , [key|j    Select the next message|]     messageSelectDown
    , [key|Down Select the next message|]     messageSelectDown
    , [key|o    Open all URLs in the selected message|] openSelectedMessageURLs
    , [key|e    Begin editing the selected message|] beginUpdateMessage
    , [key|y    Copy a verbatim section to the clipboard|] copyVerbatimToClipboard
    , [key|r    Begin composing a reply to the selected message|] beginReplyCompose
    , [key|d    Delete the selected message (with confirmation)|]
                    beginConfirmDeleteSelectedMessage

    , KB (T.pack $ "Move the cursor up by " <> show messagesPerPageOperation <> " messages")
         (Vty.EvKey Vty.KPageUp []) $
         messageSelectUpBy messagesPerPageOperation

    , KB (T.pack $ "Move the cursor down by " <> show messagesPerPageOperation <> " messages")
         (Vty.EvKey Vty.KPageDown []) $
         messageSelectDownBy messagesPerPageOperation
    ]

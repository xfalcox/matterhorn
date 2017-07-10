{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Events.ChannelScroll where

import Prelude ()
import Prelude.Compat

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

channelScrollKeybindings :: [Keybinding]
channelScrollKeybindings =
  [ [key|C-b Load more messages in the current channel          |] loadMoreMessages
  , [key|C-o Select and open a URL posted to the current channel|] startUrlSelect
  , [key|Esc      Cancel scrolling and return to channel view   |] (csMode .= Main)
  , [key|PageUp   Scroll up       |] channelPageUp
  , [key|PageDown Scroll down     |] channelPageDown
  , [key|Home     Scroll to top   |] channelScrollToTop
  , [key|End      Scroll to bottom|] channelScrollToBottom
  ]

onEventChannelScroll :: Vty.Event -> MH ()
onEventChannelScroll (Vty.EvResize _ _) = do
    cId <- use csCurrentChannelId
    mh $ do
      invalidateCache
      let vp = ChannelMessages cId
      vScrollToEnd $ viewportScroll vp
onEventChannelScroll e
  | Just kb <- lookupKeybinding e channelScrollKeybindings = kbAction kb
onEventChannelScroll _ = do
    return ()

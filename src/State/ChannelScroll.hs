module State.ChannelScroll
  (
    channelScrollToTop
  , channelScrollToBottom
  , channelScrollUp
  , channelScrollDown
  , channelPageUp
  , channelPageDown
  , loadMoreMessages
  )
 where

import           Prelude ()
import           Prelude.MH

import           Brick.Main

import           Constants
import           State.Messages
import           Types


channelPageUp :: MH ()
channelPageUp = do
    ch <- use csCurrentChannelHandle
    mh $ vScrollBy (viewportScroll (ChannelMessages ch)) (-1 * pageAmount)

channelPageDown :: MH ()
channelPageDown = do
    ch <- use csCurrentChannelHandle
    mh $ vScrollBy (viewportScroll (ChannelMessages ch)) pageAmount

channelScrollUp :: MH ()
channelScrollUp = do
    ch <- use csCurrentChannelHandle
    mh $ vScrollBy (viewportScroll (ChannelMessages ch)) (-1)

channelScrollDown :: MH ()
channelScrollDown = do
    ch <- use csCurrentChannelHandle
    mh $ vScrollBy (viewportScroll (ChannelMessages ch)) 1

channelScrollToTop :: MH ()
channelScrollToTop = do
    ch <- use csCurrentChannelHandle
    mh $ vScrollToBeginning (viewportScroll (ChannelMessages ch))

channelScrollToBottom :: MH ()
channelScrollToBottom = do
    ch <- use csCurrentChannelHandle
    mh $ vScrollToEnd (viewportScroll (ChannelMessages ch))

loadMoreMessages :: MH ()
loadMoreMessages = whenMode ChannelScroll asyncFetchMoreMessages

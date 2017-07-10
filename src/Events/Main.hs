{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}

module Events.Main where

import Prelude ()
import Prelude.Compat

import Brick
import Brick.Widgets.Edit
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import Types.Channels (ccInfo, cdType)
import State
import State.Editing
import Command
import Completion
import InputHistory
import HelpTopics (mainHelpTopic)

import Network.Mattermost (Type(..))

onEventMain :: Vty.Event -> MH ()
onEventMain e | Just kb <- lookupKeybinding e mainKeybindings = kbAction kb
onEventMain (Vty.EvPaste bytes) = handlePaste bytes
onEventMain e = handleEditingInput e

mainKeybindings :: [Keybinding]
mainKeybindings =
    [ [key|F1      Show this help screen|] $ showHelpScreen mainHelpTopic
    , [key|C-s     Select a message to edit/reply/delete|] beginMessageSelect
    , [key|C-r     Reply to the most recent message|] replyToLatestMessage
    , [key|M-p     Toggle message preview|] toggleMessagePreview
    , [key|M-k     Invoke *$EDITOR* to edit the current message|] invokeExternalEditor
    , [key|C-g     Enter fast channel selection mode|] beginChannelSelect
    , [key|C-q     Quit|] requestQuit
    , [key|Tab     Tab-complete forward|] $ tabComplete Forwards
    , [key|BackTab Tab-complete backward|] $ tabComplete Backwards

    , [key|Up Scroll up in the channel input history|] $ do
             -- Up in multiline mode does the usual thing; otherwise we
             -- navigate the history.
             isMultiline <- use (csEditState.cedMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed csCmdLine handleEditorEvent
                                           (Vty.EvKey Vty.KUp [])
                 False -> channelHistoryBackward

    , [key|Down Scroll down in the channel input history|] $ do
             -- Down in multiline mode does the usual thing; otherwise
             -- we navigate the history.
             isMultiline <- use (csEditState.cedMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed csCmdLine handleEditorEvent
                                           (Vty.EvKey Vty.KDown [])
                 False -> channelHistoryForward

    , [key|PageUp Page up in the channel message list|] $ do
             cId <- use csCurrentChannelId
             let vp = ChannelMessages cId
             mh $ invalidateCacheEntry vp
             mh $ vScrollToEnd $ viewportScroll vp
             mh $ vScrollBy (viewportScroll vp) (-1 * pageAmount)
             csMode .= ChannelScroll

    , [key|C-n   Change to the next channel in the channel list|] nextChannel
    , [key|C-p   Change to the previous channel in the channel list|] prevChannel
    , [key|M-a   Change to the next channel with unread messages|] nextUnreadChannel
    , [key|M-s   Change to the most recently-focused channel|] recentChannel

    , [key|Enter Send the current message|] $ do
             isMultiline <- use (csEditState.cedMultiline)
             case isMultiline of
                 -- Enter in multiline mode does the usual thing; we
                 -- only send on Enter when we're outside of multiline
                 -- mode.
                 True -> mhHandleEventLensed csCmdLine handleEditorEvent
                                           (Vty.EvKey Vty.KEnter [])
                 False -> do
                   csCurrentCompletion .= Nothing
                   handleInputSubmission

    , [key|M-o  Select and open a URL posted to the current channel|] startUrlSelect
    , [key|M-e  Toggle multi-line message compose mode|] toggleMultilineEditing
    , [key|Esc  Cancel message reply or update|] cancelReplyOrEdit
    , [key|C-c  Cancel message reply or update|] cancelReplyOrEdit
    ]

handleInputSubmission :: MH ()
handleInputSubmission = do
  cmdLine <- use csCmdLine
  cId <- use csCurrentChannelId

  -- send the relevant message
  mode <- use (csEditState.cedEditMode)
  let (line:rest) = getEditContents cmdLine
      allLines = T.intercalate "\n" $ line : rest

  -- We clean up before dispatching the command or sending the message
  -- since otherwise the command could change the state and then doing
  -- cleanup afterwards could clean up the wrong things.
  csCmdLine                     %= applyEdit Z.clearZipper
  csInputHistory                %= addHistoryEntry allLines cId
  csInputHistoryPosition.at cId .= Nothing
  csEditState.cedEditMode       .= NewPost

  case T.uncons line of
    Just ('/',cmd) -> dispatchCommand cmd
    _              -> sendMessage mode allLines

tabComplete :: Completion.Direction -> MH ()
tabComplete dir = do
  st <- use id
  let completableChannels = catMaybes (flip map (st^.csNames.cnChans) $ \cname -> do
          -- Only permit completion of channel names for non-Group channels
          cId <- st^.csNames.cnToChanId.at cname
          let cType = st^?csChannel(cId).ccInfo.cdType
          case cType of
              Just Group -> Nothing
              _          -> Just cname
          )

      priorities  = [] :: [T.Text]-- XXX: add recent completions to this
      completions = Set.fromList (st^.csNames.cnUsers ++
                                  completableChannels ++
                                  map (T.singleton userSigil <>) (st^.csNames.cnUsers) ++
                                  map (T.singleton normalChannelSigil <>) completableChannels ++
                                  map ("/" <>) (commandName <$> commandList))

      line        = Z.currentLine $ st^.csCmdLine.editContentsL
      curComp     = st^.csCurrentCompletion
      (nextComp, alts) = case curComp of
          Nothing -> let cw = currentWord line
                     in (Just cw, filter (cw `T.isPrefixOf`) $ Set.toList completions)
          Just cw -> (Just cw, filter (cw `T.isPrefixOf`) $ Set.toList completions)

      mb_word     = wordComplete dir priorities completions line curComp
  csCurrentCompletion .= nextComp
  csEditState.cedCompletionAlternatives .= alts
  let (edit, curAlternative) = case mb_word of
          Nothing -> (id, "")
          Just w -> (Z.insertMany w . Z.deletePrevWord, w)

  csCmdLine %= (applyEdit edit)
  csEditState.cedCurrentAlternative .= curAlternative

module Async
  ( handleAsyncRequest
  )
where

import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import qualified Data.Foldable as F
import           Data.Maybe (isNothing)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import           Data.Time (getCurrentTime, addUTCTime)
import           Text.Aspell (AspellResponse(..), mistakeWord, askAspell)
import           Lens.Micro.Platform

import           Network.Mattermost.Types
import           Network.Mattermost.Lenses
import           Network.Mattermost.Endpoints

import           Constants
import           Types
import           Types.Channels
import           Types.Messages
import           Types.Users
import           Types.Posts
import           State.Common

handleAsyncRequest :: Session -> AsyncRequest -> IO (MH ())

handleAsyncRequest _ (GenericAsync h) = h

handleAsyncRequest session (GetPostReactions cId pId) = do
    rs <- F.toList <$> mmGetReactionsForPost pId session
    return $ addReactions cId rs

handleAsyncRequest session (FetchAttachment host cId pId fId) = do
    info <- mmGetMetadataForFile fId session
    let scheme = "https://"
        attUrl = scheme <> host <> urlForFile fId
        attachment = mkAttachment (fileInfoName info) attUrl fId
        addIfMissing a as =
            if isNothing $ Seq.elemIndexL a as
            then a Seq.<| as
            else as
        addAttachment m
          | m^.mPostId == Just pId =
            m & mAttachments %~ (addIfMissing attachment)
          | otherwise              = m
    return $
      csChannel(cId).ccContents.cdMessages.traversed %= addAttachment

handleAsyncRequest _ (SpellCheck checker contents) = do
    let query = concat <$> mapM (askAspell checker) contents
        postMistakes :: [AspellResponse] -> MH ()
        postMistakes responses = do
            let getMistakes AllCorrect = []
                getMistakes (Mistakes ms) = mistakeWord <$> ms
                allMistakes = S.fromList $ concat $ getMistakes <$> responses
            csEditState.cedMisspellings .= allMistakes

    tryMM query (return . postMistakes)

handleAsyncRequest _ (SetTimeZone newTz) = return $ timeZone .= newTz

handleAsyncRequest _ (LogErrorMessage msg) = return $ mhError msg

handleAsyncRequest _ ExpireUserTypingStates = return $ do
    now <- liftIO getCurrentTime
    let expiry = addUTCTime (- userTypingExpiryInterval) now
    let expireUsers c = c & ccInfo.cdTypingUsers %~ expireTypingUsers expiry
    csChannels . mapped %= expireUsers

handleAsyncRequest session (HandleNewUsers newUserIds) = do
    nUsers <- mmGetUsersByIds newUserIds session
    let usrInfo u = userInfoFromUser u True
        usrList = F.toList nUsers
    return $ mapM_ addNewUser $ usrInfo <$> usrList

handleAsyncRequest session (SendMessage mode chanId msg) = do
    case mode of
      NewPost -> do
          let pendingPost = rawPost msg chanId
          void $ mmCreatePost pendingPost session
      Replying _ p -> do
          let pendingPost = (rawPost msg chanId) { rawPostRootId = postRootId p <|> (Just $ postId p) }
          void $ mmCreatePost pendingPost session
      Editing p -> do
          void $ mmUpdatePost (postId p) (postUpdate msg) session

    return $ return ()

handleAsyncRequest session (SetChannelTopic cId msg) = do
    let patch = defaultChannelPatch { channelPatchHeader = Just msg }
    void $ mmPatchChannel cId patch session
    return $ return ()

handleAsyncRequest session (FetchThread pId) = do
    p <- mmGetThread pId session
    let postMap = HM.fromList [ ( pstId
                                , clientPostToMessage
                                  (toClientPost x (x^.postParentIdL))
                                )
                              | (pstId, x) <- HM.toList (p^.postsPostsL)
                              ]
    return $ csPostMap %= HM.union postMap

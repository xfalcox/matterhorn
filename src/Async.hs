module Async
  ( handleAsyncRequest
  )
where

import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid ((<>))
import qualified Data.Foldable as F
import           Data.Maybe (isNothing)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import           Data.Time (getCurrentTime, addUTCTime)
import           Text.Aspell (AspellResponse(..), mistakeWord, askAspell)
import           Lens.Micro.Platform

import           Network.Mattermost.Types
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

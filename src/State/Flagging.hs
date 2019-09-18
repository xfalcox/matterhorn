module State.Flagging
  ( loadFlaggedMessages
  , updateMessageFlag
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick ( invalidateCacheEntry )
import           Data.Function ( on )
import qualified Data.Set as Set
import           Lens.Micro.Platform

import           Network.Mattermost.Types

import           State.Common
import           Types


loadFlaggedMessages :: Seq FlaggedPost -> ChatState -> IO ()
loadFlaggedMessages prefs st = doAsyncWithIO Normal st $ do
  return $ Just $ do
      sequence_ [ updateMessageFlag (flaggedPostId fp) True
                | fp <- toList prefs
                , flaggedPostStatus fp
                ]


-- | Update the UI to reflect the flagged/unflagged state of a
-- message. This __does not__ talk to the Mattermost server, but
-- rather is the function we call when the Mattermost server notifies
-- us of flagged or unflagged messages.
updateMessageFlag :: PostId -> Bool -> MH ()
updateMessageFlag pId becameFlagged = do
  if becameFlagged
    then csResources.crFlaggedPosts %= Set.insert pId
    else csResources.crFlaggedPosts %= Set.delete pId
  msgMb <- use (csPostMap.at(pId))
  case msgMb of
    Just msg
      | Just post <- msg^.mOriginalPost -> do
          (mainRef, otherRefs) <- getChanRefsFor post

          updateMessageFlagInChannel mainRef pId becameFlagged
          forM_ otherRefs $ \r ->
              updateMessageFlagInChannel r pId becameFlagged

          updateMessageFlagInPostOverlay msg becameFlagged
    _ -> return ()

updateMessageFlagInChannel :: ChanRef -> PostId -> Bool -> MH ()
updateMessageFlagInChannel cr pId becameFlagged = do
    let isTargetMessage m = m^.mMessageId == Just (MessagePostId pId)
    csChannel(cr).ccContents.cdMessages.traversed.filtered isTargetMessage.mFlagged .= becameFlagged
    csPostMap.ix(pId).mFlagged .= becameFlagged
    mh $ invalidateCacheEntry (ChannelMessages cr)

updateMessageFlagInPostOverlay :: Message -> Bool -> MH ()
updateMessageFlagInPostOverlay msg becameFlagged = do
    -- We also want to update the post overlay if this happens while
    -- we're we're observing it
    mode <- gets appMode
    case mode of
        PostListOverlay PostListFlagged
            | becameFlagged ->
                csPostListOverlay.postListPosts %=
                  addMessage (msg & mFlagged .~ True)

            -- deleting here is tricky, because it means that we need to
            -- move the focus somewhere: we'll try moving it _up_ unless
            -- we can't, in which case we'll try moving it down.
            | otherwise -> do
                selId <- use (csPostListOverlay.postListSelected)
                posts <- use (csPostListOverlay.postListPosts)
                let nextId = case getNextPostId selId posts of
                      Nothing -> getPrevPostId selId posts
                      Just x  -> Just x
                csPostListOverlay.postListSelected .= nextId
                csPostListOverlay.postListPosts %=
                  filterMessages (((/=) `on` _mMessageId) msg)
        _ -> return ()

module State.Reactions
  ( asyncFetchReactionsForPost
  , addReactions
  , removeReaction
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main ( invalidateCacheEntry )
import qualified Data.Map.Strict as Map
import           Lens.Micro.Platform
import qualified Data.Set as S

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           State.Async
import           State.Common ( fetchMentionedUsers )
import           Types


asyncFetchReactionsForPost :: Post -> MH ()
asyncFetchReactionsForPost p
  | not (p^.postHasReactionsL) = return ()
  | otherwise = do
      let cId = p^.postChannelIdL
      doAsyncChannelMM Normal cId
        (\s _ _ -> fmap toList (mmGetReactionsForPost (p^.postIdL) s))
        (\_ rs -> Just $ addReactions rs)

addReactions :: [Reaction] -> MH ()
addReactions rs = do
    let mentions = S.fromList $ UserIdMention <$> reactionUserId <$> rs
    fetchMentionedUsers mentions

    forM_ rs addReaction

addReaction :: Reaction -> MH ()
addReaction r = do
    result <- getChanRefsForPostId (r^.reactionPostIdL)
    case result of
        Nothing -> return ()
        Just (mainRef, otherRefs) ->
            forM_ (mainRef:otherRefs) $ \cr -> do
                mh $ invalidateCacheEntry $ ChannelMessages cr
                csChannel(cr).ccContents.cdMessages %= fmap upd

  where upd msg = msg & mReactions %~ insertAll (msg^.mMessageId)
        insert mId re
          | mId == Just (MessagePostId (re^.reactionPostIdL)) =
              Map.insertWith S.union (re^.reactionEmojiNameL) (S.singleton $ re^.reactionUserIdL)
          | otherwise = id
        insertAll mId msg = foldr (insert mId) msg [r]

removeReaction :: Reaction -> MH ()
removeReaction r = do
    result <- getChanRefsForPostId (r^.reactionPostIdL)
    case result of
        Nothing -> return ()
        Just (mainRef, otherRefs) ->
            forM_ (mainRef:otherRefs) $ \cr -> do
                mh $ invalidateCacheEntry $ ChannelMessages cr
                csChannel(cr).ccContents.cdMessages %= fmap upd

  where upd m | m^.mMessageId == Just (MessagePostId $ r^.reactionPostIdL) =
                  m & mReactions %~ (Map.alter delReaction (r^.reactionEmojiNameL))
              | otherwise = m
        delReaction mUs = S.delete (r^.reactionUserIdL) <$> mUs

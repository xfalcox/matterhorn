module State.UserListOverlay
  ( enterChannelMembersUserList
  , enterChannelInviteUserList
  , enterDMSearchUserList

  , userListSelectDown
  , userListSelectUp
  , userListPageDown
  , userListPageUp
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Brick.Widgets.List as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Vector as Vec
import           Data.List ( nub, sortBy )
import           Data.Ord ( comparing )
import           Lens.Micro.Platform ( (.~), (^?) )

import qualified Network.Mattermost.Endpoints as MM
import qualified Network.Mattermost.Types.Config as MM
import           Network.Mattermost.Types

import           State.Channels ( createOrFocusDMChannel, addUserToCurrentChannel )
import           State.ListOverlay
import           Types
import           Types.Common ( sanitizeUserText )


-- | Show the user list overlay for searching/showing members of the
-- current channel.
enterChannelMembersUserList :: MH ()
enterChannelMembersUserList = do
    cr <- use csCurrentChannelRef
    myId <- gets myUserId
    case cr of
        ServerChannel cId -> do
            myTId <- gets myTeamId
            enterUserListMode (ChannelMembers cId myTId)
              (\u -> case u^.uiId /= myId of
                True -> createOrFocusDMChannel u Nothing >> return True
                False -> return False
              )
        ConversationChannel cId pId -> do
            enterUserListMode (ConversationParticipants cId pId)
              (\u -> case u^.uiId /= myId of
                True -> createOrFocusDMChannel u Nothing >> return True
                False -> return False
              )

-- | Show the user list overlay for showing users that are not members
-- of the current channel for the purpose of adding them to the
-- channel.
enterChannelInviteUserList :: MH ()
enterChannelInviteUserList = do
    cr <- use csCurrentChannelRef
    let cId = case cr of
            ServerChannel i -> i
            ConversationChannel i _ -> i
    myId <- gets myUserId
    myTId <- gets myTeamId
    enterUserListMode (ChannelNonMembers cId myTId)
      (\u -> case u^.uiId /= myId of
        True -> addUserToCurrentChannel u >> return True
        False -> return False
      )

-- | Show the user list overlay for showing all users for the purpose of
-- starting a direct message channel with another user.
enterDMSearchUserList :: MH ()
enterDMSearchUserList = do
    myId <- gets myUserId
    myTId <- gets myTeamId
    config <- use csClientConfig
    let restrictTeam = case MM.clientConfigRestrictDirectMessage <$> config of
            Just MM.RestrictTeam -> Just myTId
            _ -> Nothing
    enterUserListMode (AllUsers restrictTeam)
      (\u -> case u^.uiId /= myId of
        True -> createOrFocusDMChannel u Nothing >> return True
        False -> return False
      )

-- | Show the user list overlay with the given search scope, and issue a
-- request to gather the first search results.
enterUserListMode :: UserSearchScope -> (UserInfo -> MH Bool) -> MH ()
enterUserListMode scope enterHandler =
    enterListOverlayMode csUserListOverlay UserListOverlay scope enterHandler getUserSearchResults

userInfoFromPair :: User -> Text -> UserInfo
userInfoFromPair u status =
    userInfoFromUser u True & uiStatus .~ statusFromText status

-- | Move the selection up in the user list overlay by one user.
userListSelectUp :: MH ()
userListSelectUp = userListMove L.listMoveUp

-- | Move the selection down in the user list overlay by one user.
userListSelectDown :: MH ()
userListSelectDown = userListMove L.listMoveDown

-- | Move the selection up in the user list overlay by a page of users
-- (userListPageSize).
userListPageUp :: MH ()
userListPageUp = userListMove (L.listMoveBy (-1 * userListPageSize))

-- | Move the selection down in the user list overlay by a page of users
-- (userListPageSize).
userListPageDown :: MH ()
userListPageDown = userListMove (L.listMoveBy userListPageSize)

-- | Transform the user list results in some way, e.g. by moving the
-- cursor, and then check to see whether the modification warrants a
-- prefetch of more search results.
userListMove :: (L.List Name UserInfo -> L.List Name UserInfo) -> MH ()
userListMove = listOverlayMove csUserListOverlay

-- | The number of users in a "page" for cursor movement purposes.
userListPageSize :: Int
userListPageSize = 10

getUserSearchResults :: ChatState
                     -- ^ Chat state
                     -> UserSearchScope
                     -- ^ The scope to search
                     -> Session
                     -- ^ The connection session
                     -> Text
                     -- ^ The search string
                     -> IO (Vec.Vector UserInfo)
getUserSearchResults st (ConversationParticipants cId pId) s searchString = do
    let uIds = nub $ catMaybes $ F.toList $ messageAuthor <$> msgs
        cr = ConversationChannel cId pId
        msgs = chan^.ccContents.cdMessages
        Just chan = st^?csChannel(cr)
        messageAuthor m =
            case m^.mUser of
                UserI _ uId -> Just uId
                _ -> Nothing

    users <- MM.mmGetUsersByIds (Seq.fromList uIds) s

    let uList = toList users
        matchList = filter matchUser uList
        matchUser u =
            let f g = T.toLower searchString `T.isInfixOf` g u
            in any f [ userUsername
                     , sanitizeUserText . userNickname
                     , sanitizeUserText . userFirstName
                     , sanitizeUserText . userLastName
                     ]

    case null matchList of
        False -> do
            statuses <- MM.mmGetUserStatusByIds (Seq.fromList uIds) s
            let statusMap = HM.fromList [ (statusUserId e, statusStatus e) | e <- toList statuses ]
                usersWithStatus = [ userInfoFromPair u (fromMaybe "" $ HM.lookup (userId u) statusMap)
                                  | u <- matchList
                                  ]

            return $ Vec.fromList $ sortBy (comparing _uiName) usersWithStatus
        True -> return mempty

getUserSearchResults _ scope s searchString = do
    -- Unfortunately, we don't get pagination control when there is a
    -- search string in effect. We'll get at most 100 results from a
    -- search.
    let query = UserSearch { userSearchTerm = if T.null searchString then " " else searchString
                           -- Hack alert: Searching with the string " "
                           -- above is a hack to use the search
                           -- endpoint to get "all users" instead of
                           -- those matching a particular non-empty
                           -- non-whitespace string. This is because
                           -- only the search endpoint provides a
                           -- control to eliminate deleted users from
                           -- the results. If we don't do this, and
                           -- use the /users endpoint instead, we'll
                           -- get deleted users in those results and
                           -- then those deleted users will disappear
                           -- from the results once the user enters a
                           -- non-empty string string.
                           , userSearchAllowInactive = False
                           , userSearchWithoutTeam = False
                           , userSearchInChannelId = case scope of
                               ChannelMembers cId _ -> Just cId
                               _                    -> Nothing
                           , userSearchNotInTeamId = Nothing
                           , userSearchNotInChannelId = case scope of
                               ChannelNonMembers cId _ -> Just cId
                               _                       -> Nothing
                           , userSearchTeamId = case scope of
                               AllUsers tId            -> tId
                               ChannelMembers _ tId    -> Just tId
                               ChannelNonMembers _ tId -> Just tId
                               ConversationParticipants {} ->
                                   error $ "BUG: getUserSearchResults should not be asking " <>
                                           "the server about conversation participants"
                           }
    users <- MM.mmSearchUsers query s

    let uList = toList users
        uIds = userId <$> uList

    -- Now fetch status info for the users we got.
    case null uList of
        False -> do
            statuses <- MM.mmGetUserStatusByIds (Seq.fromList uIds) s
            let statusMap = HM.fromList [ (statusUserId e, statusStatus e) | e <- toList statuses ]
                usersWithStatus = [ userInfoFromPair u (fromMaybe "" $ HM.lookup (userId u) statusMap)
                                  | u <- uList
                                  ]

            return $ Vec.fromList usersWithStatus
        True -> return mempty

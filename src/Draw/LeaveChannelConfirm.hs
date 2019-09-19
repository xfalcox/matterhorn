{-# LANGUAGE OverloadedStrings #-}
module Draw.LeaveChannelConfirm
    ( drawLeaveChannelConfirm
    )
where

import Prelude ()
import Prelude.MH

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.List ( sortBy )
import Data.Ord ( comparing )

import Draw.Main
import Themes
import Types


drawLeaveChannelConfirm :: ChatState -> [Widget Name]
drawLeaveChannelConfirm st =
    confirmBox st : (drawMain False st)

confirmBox :: ChatState -> Widget Name
confirmBox st =
    let cName = st^.csCurrentChannel.ccInfo.cdName
        isConvoFor cId (otherCr, _) = case otherCr of
            ConversationChannel otherCid _ -> otherCid == cId
            _ -> False
        cr = st^.csCurrentChannelRef
        convs = case cr of
            ServerChannel thisCid ->
                sortBy (comparing (_cdName . _ccInfo . snd)) $
                filteredChannels (isConvoFor thisCid) (st^.csChannels)
            _ -> []
        ppConv (_, cc) = hCenter $ withDefAttr dialogEmphAttr $ txt $ cc^.ccInfo.cdName
    in centerLayer $ hLimit 50 $
       withDefAttr dialogAttr $
       borderWithLabel (txt "Confirm Leave Channel") $
       vBox [ padBottom (Pad 1) $ hCenter $ txt "Are you sure you want to leave this channel?"
            , padBottom (Pad 1) $ hCenter $ withDefAttr dialogEmphAttr $ txt cName
            , if null convs
              then emptyWidget
              else padBottom (Pad 1) $
                   vBox [ hCenter $ txt "Leaving this channel will also close the "
                        , padBottom (Pad 1) $ hCenter $ txt "following conversations that you are following:"
                        , vBox $ ppConv <$> convs
                        ]
            , hCenter $ txt "Press " <+> (withDefAttr dialogEmphAttr $ txt "Y") <+> txt " to leave the channel"
            , hCenter $ txt "or any other key to cancel."
            ]

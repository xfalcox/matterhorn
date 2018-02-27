{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Draw.GenericListOverlay where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Trans.Reader (withReaderT)
import qualified Data.Foldable as F
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Lens.Micro.Platform
import           Network.Mattermost.Types
import           Network.Mattermost.Lenses

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List

import Themes
import Types
import Types.Channels
import Types.Messages
import Types.Users
import Draw.Main
import Draw.Messages
import Draw.Util

hLimitWithPadding :: Int -> Widget n -> Widget n
hLimitWithPadding pad contents = Widget
  { hSize  = Fixed
  , vSize  = (vSize contents)
  , render =
      withReaderT (& availWidthL  %~ (\ n -> n - (2 * pad))) $ render $ cropToContext contents
  }

drawGenericListOverlay :: ChatState -> [Widget Name]
drawGenericListOverlay st =
  drawListBox (st^.csGenericListOverlay) : (forceAttr "invalid" <$> drawMain st)

-- | Draw a PostListOverlay as a floating overlay on top of whatever
-- is rendered beneath it
drawListBox :: GenericListOverlayState -> Widget Name
drawListBox GenericListOverlayState { .. } =
  centerLayer $ hLimitWithPadding 10 $ borderWithLabel (txt genericListOverlayName) $
    padRight (Pad 1) messageListContents
  where messageListContents = renderList
          (\ _ e -> Brick.txt (genericListOverlayShow e))
          True
          genericListOverlayList

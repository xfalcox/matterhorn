{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Draw.GenericListOverlay where

import           Prelude ()
import           Prelude.Compat

import           Lens.Micro.Platform

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List

import Types
import Draw.Main
import Draw.Util

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

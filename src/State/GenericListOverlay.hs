{-# LANGUAGE RecordWildCards #-}

module State.GenericListOverlay where

import qualified Brick.Widgets.List as Brick
import Lens.Micro.Platform

import Types

genericListExit :: MH ()
genericListExit = do
  csGenericListOverlay .= emptyGenericListOverlay
  setMode Main

genericListSelectUp :: MH ()
genericListSelectUp = do
  overlay <- use csGenericListOverlay
  case overlay of
    GenericListOverlayState { .. } ->
      csGenericListOverlay .= GenericListOverlayState
        { genericListOverlayList = Brick.listMoveUp genericListOverlayList
        , ..
        }

genericListSelectDown :: MH ()
genericListSelectDown = do
  overlay <- use csGenericListOverlay
  case overlay of
    GenericListOverlayState { .. } ->
      csGenericListOverlay .= GenericListOverlayState
        { genericListOverlayList = Brick.listMoveDown genericListOverlayList
        , ..
        }

genericListActivate :: MH ()
genericListActivate = do
  overlay <- use csGenericListOverlay
  case overlay of
    GenericListOverlayState { .. } ->
      case Brick.listSelectedElement genericListOverlayList of
        Nothing -> return ()
        Just (_, x) -> genericListOverlayCallback x
  genericListExit

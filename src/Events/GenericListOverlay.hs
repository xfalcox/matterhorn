module Events.GenericListOverlay where

import qualified Graphics.Vty as Vty

import Types
import Events.Keybindings
import State.GenericListOverlay

onEventGenericListOverlay :: Vty.Event -> MH ()
onEventGenericListOverlay =
  handleKeyboardEvent genericListOverlayKeybindings $ \_ -> return ()

-- | The keybindings we want to use while viewing a user list overlay
genericListOverlayKeybindings :: KeyConfig -> [Keybinding]
genericListOverlayKeybindings = mkKeybindings
  [ mkKb CancelEvent "Close the list" genericListExit
  , mkKb SelectUpEvent "Select the previous item" genericListSelectUp
  , mkKb SelectDownEvent "Select the next item" genericListSelectDown
  , mkKb ActivateListItemEvent "Use the selected list item" genericListActivate
  ]

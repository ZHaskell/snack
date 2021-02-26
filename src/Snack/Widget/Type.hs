module Snack.Widget.Type (
  SnackWidget(..)
) where

import Brick.Widgets.Edit (Editor)
import Snack.Event.Completion (CompletionFunc)

data SnackWidget n = MkSnackWidget
  { name :: n,
    extent :: Maybe (Int, Int),
    snackEditor :: Editor String n,
    snackEditor' :: Editor String n,
    completionBox :: (String, [String], [String]),
    completionInsert :: (String, [String], [String]),
    completionFunc :: CompletionFunc IO,
    isInCompletion :: Bool
  }

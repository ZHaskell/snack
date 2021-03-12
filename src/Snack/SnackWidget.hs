module Snack.SnackWidget
  ( SnackWidget (..),
    newWidget,
  )
where

import Snack.CommandLine
import Snack.Completion.Event (CompletionFunc)
import Snack.Completion.State

-- | The datatype records all the essential info to render the terminal
data SnackWidget n = SnackWidget
  { name :: n,
    extent :: Maybe (Int, Int),
    commandLine :: CommandLine String n,
    completionState :: CompletionState,
    completionFunc :: CompletionFunc IO
  }

newWidget :: n -> n -> CompletionFunc IO -> SnackWidget n
newWidget n m complete = SnackWidget n Nothing (initialCommandLine m) initialCompletionState complete

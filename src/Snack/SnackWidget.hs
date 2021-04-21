module Snack.SnackWidget
  ( SnackWidget (..),
    newWidget,
    SnackSettings (..)
  )
where

import           Brick.BChan            (BChan)
import           Snack.CommandLine      (CommandLine, initialCommandLine)
import           Snack.Completion.Event (CompletionFunc)
import           Snack.Completion.State (CompletionState,
                                         initialCompletionState)
import           Snack.Parser           (Parser)

-- | The datatype records all the essential info to render the terminal
data SnackWidget n a = SnackWidget
  { extent          :: Maybe (Int, Int),
    commandLine     :: CommandLine String n,
    completionState :: CompletionState,
    snackSettings   :: SnackSettings n a
  }

newWidget :: n -> SnackSettings n a -> SnackWidget n a
newWidget n=
  SnackWidget Nothing (initialCommandLine n) initialCompletionState

data SnackSettings n a = Settings
  { widgetName     :: n,
    commandsList   :: [(String, Parser a)],
    readChan       :: BChan a,
    completionFunc :: CompletionFunc IO
  }

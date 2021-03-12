module Snack.Completion.State
  ( presentCompletion,
    getCompletionReplacement,
    cycleCompletion,
    CompletionState (..),
    initialCompletionState,
  )
where

import Snack.Completion.Event

data CompletionState = CompletionState
  { -- | Text to display
    completionBox :: (String, [String], [String]),
    -- | Text to insert
    completionInsert :: (String, [String], [String]),
    -- | when there are multiple completion, whether it is in tab completion selection
    isInCompletion :: Bool
  }

-- | initailize completion state
initialCompletionState :: CompletionState
initialCompletionState =
  CompletionState
    { completionBox = ("", [], []),
      completionInsert = ("", [], []),
      isInCompletion = False
    }

-- | get the text to display from completions
presentCompletion :: [Completion] -> (String, [String], [String])
presentCompletion = menuCompletion

menuCompletion :: [Completion] -> (String, [String], [String])
menuCompletion [] = ("", [], [])
menuCompletion [c] = (display c, [], [])
menuCompletion (c : cs) = (display c, [], map display cs)

-- | get the text to insert from the completions
getCompletionReplacement :: [Completion] -> (String, [String], [String])
getCompletionReplacement [] = ("", [], [])
getCompletionReplacement [c] = (useCompletion c, [], [])
getCompletionReplacement (c : cs) = (useCompletion c, [], map useCompletion cs)

-- | cycle the tab completion
cycleCompletion :: (a, [a], [a]) -> (a, [a], [a])
cycleCompletion (x, [], []) = (x, [], [])
cycleCompletion (x, y : ys, []) = (y, [], ys ++ [x])
cycleCompletion (x, [], z : zs) = (z, [x], zs)
cycleCompletion (x, y : ys, z : zs) = (z, y : ys ++ [x], zs)

--listCompletion
--floatCompletion

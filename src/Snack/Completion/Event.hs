module Snack.Completion.Event
  ( CompletionFunc,
    Completion (..),
    completeWord,
    completeWordWithPrev,
    useCompletion,
    setReplacement,
    noCompletion,
    simpleCompletion,
    fallbackCompletion
  )
where

-- | Performs completions from the given line state.
--
-- The first 'String' argument is the contents of the line to the left of the cursor,
-- reversed.
-- The second 'String' argument is the contents of the line to the right of the cursor.
--
-- The output 'String' is the unused portion of the left half of the line, reversed.
type CompletionFunc m = (String, String) -> m (String, [Completion])

data Completion = Completion
  { -- | Text to insert in line.
    replacement :: String,
    -- | Text to display when listing
    -- alternatives.
    display     :: String,
    -- | Whether this word should be followed by a
    -- space, end quote, etc.
    isFinished  :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Disable completion altogether.
noCompletion :: Monad m => CompletionFunc m
noCompletion (s, _) = return (s, [])

--------------
-- Word break functions

-- | A custom 'CompletionFunc' which completes the word immediately to the left of the cursor.
--
-- A word begins either at the start of the line or after an unescaped whitespace character.
completeWord ::
  Monad m =>
  -- | An optional escape character
  Maybe Char ->
  -- | Characters which count as whitespace
  [Char] ->
  -- | Function to produce a list of possible completions
  (String -> m [Completion]) ->
  CompletionFunc m
completeWord esc ws = completeWordWithPrev esc ws . const

-- | A custom 'CompletionFunc' which completes the word immediately to the left of the cursor,
-- and takes into account the line contents to the left of the word.
--
-- A word begins either at the start of the line or after an unescaped whitespace character.
completeWordWithPrev ::
  Monad m =>
  -- | An optional escape character
  Maybe Char ->
  -- | Characters which count as whitespace
  [Char] ->
  -- | Function to produce a list of possible completions.  The first argument is the
  -- line contents to the left of the word, reversed.  The second argument is the word
  -- to be completed.
  (String -> String -> m [Completion]) ->
  CompletionFunc m
completeWordWithPrev esc ws f (line, _) = do
  let (word, rest) = case esc of
        Nothing -> break (`elem` ws) line
        Just e  -> escapedBreak e line
  completions <- f rest (reverse word)
  return (rest, map (escapeReplacement esc ws) completions)
  where
    escapedBreak e (c : d : cs)
      | d == e && c `elem` (e : ws) =
        let (xs, ys) = escapedBreak e cs in (c : xs, ys)
    escapedBreak e (c : cs)
      | c `notElem` ws =
        let (xs, ys) = escapedBreak e cs in (c : xs, ys)
    escapedBreak _ cs = ("", cs)

-- | Create a finished completion out of the given word.
simpleCompletion :: String -> Completion
simpleCompletion = completion

completion :: String -> Completion
completion str = Completion str str True

setReplacement :: (String -> String) -> Completion -> Completion
setReplacement f c = c {replacement = f $ replacement c}

escapeReplacement :: Maybe Char -> String -> Completion -> Completion
escapeReplacement esc ws f = case esc of
  Nothing -> f
  Just e  -> f {replacement = escape e (replacement f)}
  where
    escape e (c : cs)
      | c `elem` (e : ws) = e : c : escape e cs
      | otherwise = c : escape e cs
    escape _ "" = ""

-- | If the first completer produces no suggestions, fallback to the second
-- completer's output.
fallbackCompletion :: Monad m => CompletionFunc m -> CompletionFunc m -> CompletionFunc m
fallbackCompletion a b input = do
  aCompletions <- a input
  if null (snd aCompletions)
    then b input
    else return aCompletions

useCompletion :: Completion -> String
useCompletion = replacement

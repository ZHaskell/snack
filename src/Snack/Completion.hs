module Snack.Completion
  ( module Snack.Completion.State,
    module Snack.Completion.Event,
    commandOptionCompletion,
  )
where

import           Data.List                    (isPrefixOf)
import           Data.Maybe                   (listToMaybe)
import           Options.Applicative          (defaultPrefs)
import           Options.Applicative.Common   (mapParser, runParserFully,
                                               showOption)
import           Options.Applicative.Internal (runCompletion)
import           Options.Applicative.Types
import           Snack.Completion.Event       (Completion (..), CompletionFunc,
                                               completeWord,
                                               completeWordWithPrev,
                                               fallbackCompletion, noCompletion,
                                               setReplacement, simpleCompletion,
                                               useCompletion)
import           Snack.Completion.State       (CompletionState (..),
                                               cycleCompletion,
                                               getCompletionReplacement,
                                               initialCompletionState,
                                               presentCompletion)


mkCompletion :: String -> [String] -> [Completion]
mkCompletion toComplete = map complete . filter (toComplete `isPrefixOf`)
  where
    complete x = setReplacement (drop (length toComplete)) (simpleCompletion x)

commandCompletion :: [String] -> String -> String -> IO [Completion]
commandCompletion commands [] = return . flip mkCompletion commands
commandCompletion _        _  = return . return $ []

optionCompletion :: [String] -> String -> String -> IO [Completion]
optionCompletion commands _ = return . flip mkCompletion commands

completionQuery :: [(String, Parser a)] -> String -> String -> IO [Completion]
completionQuery xs prev current =
  case parser of
    Nothing -> return []
    Just p ->
      let compl = runParserFully Intersperse p (drop 1 prev')
      in case runCompletion compl defaultPrefs of
        Just (Left (SomeParser p', a)) -> listOptions a p'
        Just (Right c)                 -> mkCompletion current <$> runCompleter c current
        Nothing                        -> return []
  where
    parser = listToMaybe prev' >>= flip lookup xs
    prev'  = words $ reverse prev
    listOptions a = fmap concat . sequence . mapParser (optCompletions a)
    -- For options and flags, ensure that the  user
    -- hasn't disabled them with `--`.

    optCompletions argPolicy reachability opt = case optMain opt of
      OptReader ns _ _
        | argPolicy /= AllPositionals -> completeName ns prev current
        | otherwise                   -> return []
      FlagReader ns _
        | argPolicy /= AllPositionals -> completeName ns prev current
        | otherwise                   -> return []
      ArgReader rdr
        | argumentIsUnreachable reachability -> return []
        | otherwise                          -> mkCompletion current <$> runCompleter (crCompleter rdr) current
      CmdReader _ ns _
        | argumentIsUnreachable reachability -> return []
        | otherwise                          -> commandCompletion ns prev current

    completeName = optionCompletion . map showOption

commandOptionCompletion :: [(String, Parser a)] -> CompletionFunc IO
commandOptionCompletion xs = fallbackCompletion
  (completeWordWithPrevDefault (commandCompletion (map fst xs)))
  (completeWordWithPrevDefault (completionQuery xs))

completeWordWithPrevDefault :: (String -> String -> IO [Completion]) -> CompletionFunc IO
completeWordWithPrevDefault = completeWordWithPrev (Just '\\') " "

module CompletionExample where

import Data.List (isPrefixOf)
import Snack

data Name = Terminal1 | Editor1 deriving (Eq, Ord, Show)

snackList :: [[Char]]
snackList = ["kitkat", "twirst", "mars", "sweets", "crisps", "doritos", "pretzels", "peanuts"]

completeSnacks :: String -> IO [Completion]
completeSnacks string = return . map complete . filter (string `isPrefixOf`) $ snackList
  where
    complete x = setReplacement (drop (length string)) (simpleCompletion x)

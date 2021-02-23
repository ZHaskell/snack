module Snack.Completion (
    System.Console.Haskeline.Completion.CompletionFunc
  , System.Console.Haskeline.Completion.Completion
  , System.Console.Haskeline.Completion.simpleCompletion
  , System.Console.Haskeline.Completion.completeWord
  , setReplacement
  , menuCompletion
  , useCompletion
  , presentCompletion
  , getCompletionReplacement
) where

import System.Console.Haskeline.Completion

-- useCompletion :: Editor a -> Completion -> Editor a 
-- useCompletion = undefined
useCompletion :: Completion -> String
useCompletion c | isFinished c = replacement c ++ " "
                | otherwise   = replacement c

-- completionEvent :: 

presentCompletion :: [Completion] -> (String, [String], [String])
presentCompletion = menuCompletion 

menuCompletion :: [Completion] -> (String, [String], [String])
menuCompletion []  = ("",[],[])
menuCompletion [c] = (display c, [], [])
menuCompletion (c:cs) = (display c, [], map display cs)

getCompletionReplacement :: [Completion] -> (String, [String], [String])
getCompletionReplacement []  = ("",[],[])
getCompletionReplacement [c] = (useCompletion c, [], [])
getCompletionReplacement (c:cs) = (useCompletion c, [], map useCompletion cs)

setReplacement :: (String -> String) -> Completion -> Completion
setReplacement f c = c {replacement = f $ replacement c}
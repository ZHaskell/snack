module Snack.Widget.CompletionBox (
  presentCompletion,
  getCompletionReplacement,
  cycleCompletion
) where

import Snack.Event.Completion

presentCompletion :: [Completion] -> (String, [String], [String])
presentCompletion = menuCompletion

menuCompletion :: [Completion] -> (String, [String], [String])
menuCompletion [] = ("", [], [])
menuCompletion [c] = (display c, [], [])
menuCompletion (c : cs) = (display c, [], map display cs)

getCompletionReplacement :: [Completion] -> (String, [String], [String])
getCompletionReplacement [] = ("", [], [])
getCompletionReplacement [c] = (useCompletion c, [], [])
getCompletionReplacement (c : cs) = (useCompletion c, [], map useCompletion cs)

cycleCompletion :: (a, [a], [a]) -> (a, [a], [a])
cycleCompletion (x, [], []) = (x, [], [])
cycleCompletion (x, y : ys, []) = (y, [], ys ++ [x])
cycleCompletion (x, [], z : zs) = (z, [x], zs)
cycleCompletion (x, y : ys, z : zs) = (z, y : ys ++ [x], zs)

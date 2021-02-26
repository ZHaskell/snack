module Snack.Widget.Editor (
  cursorCol,
  cursorRow,
  toLeft,
  toRight,
  above,
  below,
  insertMany
) where

import qualified Brick.Widgets.Edit as E
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic as Z

cursorCol :: E.Editor t a -> Int
cursorCol = snd . Z.cursorPosition . E.editContents

cursorRow :: E.Editor t a -> Int
cursorRow = snd . Z.cursorPosition . E.editContents

toLeft :: (Z.GenericTextZipper a) => E.Editor a n -> a
toLeft e = (Z.take (cursorCol e) . Z.currentLine . E.editContents) e

toRight :: (Z.GenericTextZipper a) => E.Editor a n -> a
toRight e = (Z.drop (cursorCol e) . Z.currentLine . E.editContents) e

above :: (Z.GenericTextZipper a) => E.Editor a n -> a
above e = (Z.take (cursorRow e) . Z.currentLine . E.editContents) e

below :: (Z.GenericTextZipper a) => E.Editor a n -> a
below e = (Z.drop (cursorRow e + 1) . Z.currentLine . E.editContents) e

insertMany :: Monoid a => a -> E.Editor a n -> E.Editor a n
insertMany x e = e {E.editContents = Z.insertMany x (E.editContents e)}

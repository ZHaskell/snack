module Snack.CommandLine.Event where

import Data.Char (isPrint)
import qualified Snack.CommandLine.Editor as E
import qualified Snack.CommandLine.Generic as G
import qualified Snack.CommandLine.Value as V

-- | Design
-- above [Input history]
-- current is the first line of below
-- while up down will go cons the last line to the below
applyCommandLineEvent :: Monoid t => (E.CommandLineEditor t -> E.CommandLineEditor t) -> V.CommandLine t n -> V.CommandLine t n
applyCommandLineEvent f commandL = commandL {V.commandLineInput = (f . V.commandLineInput) commandL}

-- | Insert a character at the current cursor position.
--
-- If the character is a newline, break the current line.
--
-- If the character is non-printable, ignore it.
--
-- Otherwise insert the character and move the cursor one position to
-- the right.
insertChar :: G.GenericCommandLineEditor t => Char -> V.CommandLine t n -> V.CommandLine t n
insertChar ch
  | ch == '\n' = breakLine
  | isPrint ch = applyCommandLineEvent (\e -> e {E.toLeft = E.toLeft e <> (G.singleton ch)})
  | otherwise = id

-- | Insert many characters at the current cursor position. Move the
-- cursor to the end of the inserted text.
insertMany :: G.GenericCommandLineEditor t => t -> V.CommandLine t n -> V.CommandLine t n
insertMany str = go (G.toList str)
  where
    go [] = id
    go (c : cs) = go cs . insertChar c

-- | Remove all text from the cursor position to the beginning of the
-- current line.
killToBOL :: G.GenericCommandLineEditor t => V.CommandLine t n -> V.CommandLine t n
killToBOL = applyCommandLineEvent (\e -> e {E.toLeft = mempty})

killToEOL :: G.GenericCommandLineEditor t => V.CommandLine t n -> V.CommandLine t n
killToEOL = applyCommandLineEvent (\e -> e {E.toRight = mempty})

-- | Move the cursor to the end of the current line.
gotoEOL :: G.GenericCommandLineEditor t => V.CommandLine t n -> V.CommandLine t n
gotoEOL = applyCommandLineEvent (\e -> e {E.toLeft = E.currentLine e, E.toRight = mempty})

-- | Move the cursor to the beginning of the current line.
gotoBOL :: G.GenericCommandLineEditor t => V.CommandLine t n -> V.CommandLine t n
gotoBOL = applyCommandLineEvent (\e -> e {E.toLeft = mempty, E.toRight = E.currentLine e})

-- | Move the cursor to the end of a text zipper.
gotoEOF :: G.GenericCommandLineEditor t => V.CommandLine t n -> V.CommandLine t n
gotoEOF cle = applyCommandLineEvent gotoEOF' cle
  where
    gotoEOF' e =
      e
        { E.toLeft = end,
          E.toRight = mempty,
          E.above = top,
          E.below = mempty
        }
    txt = V.getCommandLineInput cle
    (top, end) =
      if null txt
        then (mempty, mempty)
        else (init txt, last txt)

-- | Move the cursor right by one position. If the cursor is at the end
-- of a line, it stops
moveRight :: G.GenericCommandLineEditor t => V.CommandLine t n -> V.CommandLine t n
moveRight commandL@(V.CommandLine input _ _ _)
  -- Are we able to keep moving right on the current line?
  | (not . G.null . V.toRight) commandL =
    commandL
      { V.commandLineInput =
          input
            { E.toLeft = V.toLeft commandL <> G.take 1 (V.toRight commandL),
              E.toRight = G.drop 1 $ V.toRight commandL
            }
      }
  | otherwise = commandL

-- | Move the cursor left by one position. If the cursor is at the end
-- of a liG.ne, it stops
moveLeft :: G.GenericCommandLineEditor t => V.CommandLine t n -> V.CommandLine t n
moveLeft commandL@(V.CommandLine input _ _ _)
  | (not . G.null . V.toLeft) commandL =
    commandL
      { V.commandLineInput =
          input
            { E.toLeft = G.init $ V.toLeft commandL,
              E.toRight = (G.singleton . G.last . V.toLeft) commandL <> V.toRight commandL
            }
      }
  | otherwise = commandL

-- | Get the previous input history of the command line (Move the cursor up by one row.)
-- If there no are rows abovG.e the current one, stop.
-- Note: The cursor will always be at thG.e end of the line after moved.
moveUp :: G.GenericCommandLineEditor t => V.CommandLine t n -> V.CommandLine t n
moveUp commandL@(V.CommandLine input _ _ _)
  -- if there is a line above, just go to the end of it
  | (not . V.isFirstLine) commandL =
    commandL
      { V.commandLineInput =
          input
            { E.above = init $ V.above commandL,
              E.below = E.currentLine input : V.below commandL,
              E.toLeft = last $ V.above commandL,
              E.toRight = mempty
            }
      }
  | otherwise = commandL

-- | Get the followed input history of the command line (Move the cursor down by one row.)
-- If there no are rows below the current one, stop.
-- Note : The cursor will always be at the end of the line after moved.
moveDown :: G.GenericCommandLineEditor t => V.CommandLine t n -> V.CommandLine t n
moveDown commandL@(V.CommandLine input _ _ _)
  -- if there is a line below, just go to the end of it
  | (not . V.isLastLine) commandL =
    commandL
      { V.commandLineInput =
          input
            { E.above = V.above commandL ++ [E.currentLine input],
              E.below = tail $ V.below commandL,
              E.toLeft = head $ V.below commandL,
              E.toRight = mempty
            }
      }
  | otherwise = commandL

-- | Delete the character preceding the cursor position, and move the
-- cursor backwards by one character.
deletePrevChar :: (Eq t, G.GenericCommandLineEditor t) => V.CommandLine t n -> V.CommandLine t n
deletePrevChar commandL
  | moveLeft commandL == commandL = commandL
  | otherwise = deleteChar $ moveLeft commandL

-- | Delete the character at the cursor position. Leaves the cursor
-- position unchanged. If the cursor is at the end of a line of text,
-- this combines the line with the line below.
deleteChar :: G.GenericCommandLineEditor t => V.CommandLine t n -> V.CommandLine t n
deleteChar commandL@(V.CommandLine input _ _ _)
  | (not . G.null . V.toRight) commandL =
    commandL
      { V.commandLineInput =
          input {E.toRight = (G.drop 1 . V.toRight) commandL}
      }
  | otherwise = commandL

-- | Insert to the command line input
insertToHistory :: G.GenericCommandLineEditor t => t -> V.CommandLine t n -> V.CommandLine t n
insertToHistory x commandL@(V.CommandLine _ _ history _) = commandL {V.commandLineHistory = history ++ [x]}

-- | Insert to the command line contents
insertToContents :: G.GenericCommandLineEditor t => t -> V.CommandLine t n -> V.CommandLine t n
insertToContents x commandL@(V.CommandLine _ contents _ _) = commandL {V.commandLineContents = contents ++ [x]}

breakLine :: G.GenericCommandLineEditor t => V.CommandLine t n -> V.CommandLine t n
breakLine commandL@(V.CommandLine _ _ history _) =
  commandL
    { V.commandLineInput =
        newInput {E.above = E.above newInput ++ [V.getCurrentLine commandL]}
    }
  where
    newInput = E.mkEditorFromHistory history

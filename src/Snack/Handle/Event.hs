{-# LANGUAGE OverloadedStrings #-}

module Snack.Handle.Event
  ( handleBrickEvent,
  )
where

import qualified Brick.Main as M
import qualified Brick.Types as T
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import Snack.CommandLine as C
import Snack.Completion
import Snack.SnackWidget

handleBrickEvent :: (Eq n) => SnackWidget n -> T.BrickEvent n e -> T.EventM n (T.Next (SnackWidget n))
handleBrickEvent swidget@(SnackWidget _ _ commandL completionS _) (T.VtyEvent event) = do
  case event of
    V.EvKey V.KEsc [] -> M.halt swidget
    V.EvKey V.KEnter [] | isInCompletion completionS -> do
      let commandL' = (insertMany . (\(x, _, _) -> x) . completionInsert) completionS commandL
      M.continue swidget {commandLine = commandL', completionState = initialCompletionState}
    V.EvKey (V.KChar '\t') [] -> handleCompletion swidget
    _ -> do
      let commandL' = (insertMany . (\(x, _, _) -> x) . completionInsert) completionS commandL
          commandL'' = handleCommandLineEvent event commandL'
      M.continue swidget {commandLine = commandL'', completionState = initialCompletionState}
handleBrickEvent swidget _ = M.continue swidget

handleCompletion :: SnackWidget n -> T.EventM n (T.Next (SnackWidget n))
handleCompletion swidget@(SnackWidget _ _ _ completionS completionF)
  | isInCompletion completionS = do
    let completionS' =
          completionS
            { completionBox = (cycleCompletion . completionBox) completionS,
              completionInsert = (cycleCompletion . completionInsert) completionS
            }
    M.continue swidget {completionState = completionS'}
  | otherwise = do
    (_, completions) <-
      liftIO $
        completionF
          ( reverse $ (C.toLeft . commandLine) swidget,
            (C.toRight . commandLine) swidget
          )
    if null completions
      then M.continue swidget
      else
        M.continue
          swidget
            { completionState =
                CompletionState
                  (presentCompletion completions)
                  (getCompletionReplacement completions)
                  True
            }

handleCommandLineEvent :: (GenericCommandLineEditor t, DecodeUtf8 t, Eq t, Monoid t) => V.Event -> CommandLine t n -> CommandLine t n
handleCommandLineEvent e = case e of
  V.EvPaste bs -> case decodeUtf8 bs of
    Left _ -> id
    Right t -> C.insertMany t
  V.EvKey V.KEnter [] -> handleMoveToNextLine
  V.EvKey (V.KChar 'a') [V.MCtrl] -> C.gotoBOL
  V.EvKey (V.KChar 'e') [V.MCtrl] -> C.gotoEOL
  V.EvKey (V.KChar 'd') [V.MCtrl] -> C.deleteChar
  V.EvKey (V.KChar 'k') [V.MCtrl] -> C.killToEOL
  V.EvKey (V.KChar 'u') [V.MCtrl] -> C.killToBOL
  V.EvKey V.KDel [] -> C.deleteChar
  V.EvKey (V.KChar c) [] | c /= '\t' -> C.insertChar c
  V.EvKey V.KUp [] -> C.moveUp
  V.EvKey V.KDown [] -> C.moveDown
  V.EvKey V.KLeft [] -> C.moveLeft
  V.EvKey V.KRight [] -> C.moveRight
  V.EvKey V.KBS [] -> C.deletePrevChar
  V.EvKey V.KHome [] -> C.gotoBOL
  V.EvKey V.KEnd [] -> C.gotoEOL
  _ -> id

handleMoveToNextLine :: (GenericCommandLineEditor t, DecodeUtf8 t, Eq t, Monoid t) => CommandLine t n -> CommandLine t n
handleMoveToNextLine commandL =
  let hasNewline = not (C.isLastLine commandL)
      lineContent = getCurrentLine commandL
   in insertToContents lineContent $
        insertToHistory lineContent $
          insertChar '\n' $
            if hasNewline
              then insertMany lineContent $ gotoEOF commandL
              else commandL

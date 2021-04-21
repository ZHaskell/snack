
{-# LANGUAGE RecordWildCards #-}
module Snack.Handle.Event
  ( handleBrickEvent,
  )
where

import           Brick.BChan            (writeBChan)
import qualified Brick.Main             as M
import qualified Brick.Types            as T
import           Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty           as V
import qualified Options.Applicative    as O
import           Snack.AppEvent         (TerminalEvent (..))
import qualified Snack.CommandLine      as C
import           Snack.Completion       (CompletionState (..), cycleCompletion,
                                         getCompletionReplacement,
                                         initialCompletionState,
                                         presentCompletion)
import           Snack.SnackWidget      (SnackSettings (commandsList, completionFunc, readChan),
                                         SnackWidget (..))


handleBrickEvent :: Eq n
                 => SnackWidget n a -> T.BrickEvent n TerminalEvent
                 -> T.EventM n (T.Next (SnackWidget n a))
handleBrickEvent swidget@(SnackWidget _ commandL completionS _) (T.VtyEvent event) =
  case event of
    V.EvKey V.KEsc [] -> M.halt swidget

    V.EvKey V.KEnter [] | isInCompletion completionS -> do
      let commandL' = (C.insertMany . (\(x, _, _) -> x) . completionInsert) completionS commandL
      M.continue swidget {commandLine = commandL', completionState = initialCompletionState}
                        | otherwise -> handleMoveToNextLine swidget

    V.EvKey (V.KChar '\t') [] -> handleCompletion swidget

    _ -> do
      let commandL' = (C.insertMany . (\(x, _, _) -> x) . completionInsert) completionS commandL
          commandL'' = handleCommandLineEvent event commandL'
      M.continue swidget {commandLine = commandL'', completionState = initialCompletionState}

handleBrickEvent swidget@(SnackWidget _ commandL _ _) (T.AppEvent event) = case event of
  PrintLn x -> M.continue swidget { commandLine = C.insertToContents x commandL }
  Interrupt -> M.halt swidget

handleBrickEvent swidget _ = M.continue swidget

handleCompletion :: SnackWidget n a -> T.EventM n (T.Next (SnackWidget n a))
handleCompletion swidget@(SnackWidget _ commandL completionS settings)
  | isInCompletion completionS = do
    let completionS' = completionS
            { completionBox = (cycleCompletion . completionBox) completionS,
              completionInsert = (cycleCompletion . completionInsert) completionS
            }
    M.continue swidget { completionState = completionS' }
  | otherwise = do
    (_, completions) <- liftIO $ completionFunc settings (reverse $ C.toLeft commandL, C.toRight commandL)
    case completions of
      [] ->  M.continue swidget
      -- [x] -> M.continue swidget {
      --   commandLine = C.insertMany (useCompletion x) commandL }
      _   -> M.continue swidget
        { completionState = CompletionState
            (presentCompletion completions)
            (getCompletionReplacement completions)
            True
        }

handleMoveToNextLine :: SnackWidget n a -> T.EventM n (T.Next (SnackWidget n a))
handleMoveToNextLine swidget@(SnackWidget _ commandL _ settings) = do
  let hasNewline = not (C.isLastLine commandL)
      lineContent = C.getCurrentLine commandL
      commandL' = C.insertToContents lineContent . C.insertToHistory lineContent . C.insertChar '\n' $
        if hasNewline then C.insertMany lineContent $ C.gotoEOF commandL else commandL
      swidget' = swidget { commandLine = commandL' }
  if not $ C.null lineContent then (do
    let (command : args) = words lineContent
    case lookup command (commandsList settings) of
      Just p -> do
        let parseResult = O.execParserPure O.defaultPrefs (O.info p (O.progDesc "")) args
        handleParseResult swidget' parseResult
      Nothing -> M.continue swidget')
    else M.continue swidget'

handleParseResult :: SnackWidget n a -> O.ParserResult a -> T.EventM n (T.Next (SnackWidget n a))
handleParseResult SnackWidget {..} (O.Success a) = do
  liftIO $ writeBChan (readChan snackSettings) a
  M.continue SnackWidget {..}
handleParseResult swidget (O.Failure f) =
  M.continue swidget {commandLine = C.insertToContents (show f) (commandLine swidget)}
handleParseResult swidget (O.CompletionInvoked f) =
  M.continue swidget {commandLine = C.insertToContents (show f) (commandLine swidget)}

handleCommandLineEvent :: (C.GenericCommandLineEditor t, C.DecodeUtf8 t, Eq t, Monoid t) => V.Event -> C.CommandLine t n -> C.CommandLine t n
handleCommandLineEvent e = case e of
  V.EvPaste bs -> case C.decodeUtf8 bs of
    Left _  -> id
    Right t -> C.insertMany t
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

module Snack.Event.HandleEvent(
  handleBrickEvent
) where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import Snack.Widget.CompletionBox
import qualified Snack.Widget.Editor as SE
import Snack.Widget.Type

handleBrickEvent :: (Eq n) => SnackWidget n -> T.BrickEvent n e -> T.EventM n (T.Next (SnackWidget n))
handleBrickEvent swidget (T.VtyEvent event) = do
  case event of
    V.EvKey V.KEsc []
      | isInCompletion swidget -> M.continue swidget {snackEditor = snackEditor' swidget, isInCompletion = False}
      | otherwise -> M.halt swidget
    V.EvKey (V.KChar '\t') [] -> handleCompletion swidget
    _ -> do
      editor <- E.handleEditorEvent event (snackEditor swidget)
      M.continue swidget {snackEditor = editor, snackEditor' = editor, completionBox = ("", [], []), isInCompletion = False}
handleBrickEvent swidget _ = M.continue swidget

handleCompletion :: SnackWidget n -> T.EventM n (T.Next (SnackWidget n))
handleCompletion swidget
  | isInCompletion swidget = do
    let (x, _, _) = completionInsert swidget
        newSnackEditor = SE.insertMany x (snackEditor' swidget)
    M.continue swidget { completionBox = cycleCompletion (completionBox swidget),
                         completionInsert = cycleCompletion (completionInsert swidget),
                         snackEditor = newSnackEditor
                       }
  | otherwise = do
    (_, completions) <- liftIO $ completionFunc swidget (reverse $ (SE.toLeft . snackEditor) swidget,
                                                        (SE.toRight . snackEditor) swidget)
    if null completions
      then M.continue swidget
      else M.continue swidget { completionBox = presentCompletion completions,
                                completionInsert = getCompletionReplacement completions,
                                isInCompletion = True
                              }
 
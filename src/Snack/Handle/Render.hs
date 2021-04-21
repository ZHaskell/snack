{-# LANGUAGE RecordWildCards #-}
module Snack.Handle.Render
  ( renderSWidget,
  )
where

import           Brick.AttrMap          (attrName)
import qualified Brick.Types            as T
import           Brick.Widgets.Core     (TextWidth (..), padLeft, reportExtent,
                                         showCursor, str, vBox, viewport,
                                         visibleRegion, withDefAttr, (<=>))
import           Snack.CommandLine      (CommandLine (commandLineName),
                                         GenericCommandLineEditor, charAtCursor,
                                         cursorRowContents,
                                         getCommandLineContents, toLeft,
                                         toRight)
import           Snack.Completion.State (CompletionState (completionBox, completionInsert))
import           Snack.SnackWidget      (SnackSettings (widgetName),
                                         SnackWidget (..))

renderCommandLine :: (Ord n, Show n, Monoid t, TextWidth t, GenericCommandLineEditor t)
                  => ([t] -> T.Widget n)
                  -> CommandLine t n -> t
                  -> T.Widget n
renderCommandLine draw cl completion =
  let cursorLoc = T.Location (textWidth (toLeft cl), cursorRowContents cl)
      name = commandLineName cl
      atChar = charAtCursor cl
      atCharWidth = maybe 1 textWidth atChar
      contents = getCommandLineContents cl
   in showCursor name cursorLoc $
        visibleRegion cursorLoc (atCharWidth, 1) $
          draw $
            contents ++ [toLeft cl <> completion <> toRight cl]

renderSWidget :: (Ord n, Show n) => SnackWidget n a -> T.Widget n
renderSWidget SnackWidget { .. } =
  reportExtent (widgetName snackSettings) $ viewport (widgetName snackSettings) T.Vertical renderedCommandLine
  where
    renderedCommandLine = renderCommandLine ((<=> completionBar) . str . unlines) commandLine selectedInsert

    completionBar =
      padLeft ((T.Pad . length . toLeft) commandLine) . withDefAttr (attrName "completionBar") $
        vBox
          ( map (withDefAttr (attrName "completionBar") . str) before
              ++ (withDefAttr (attrName "selectedCompletion") . str) selected :
            map (withDefAttr (attrName "completionBar") . str) after
          )
    (selected, before, after) = completionBox completionState
    (selectedInsert, _, _) = completionInsert completionState

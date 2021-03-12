module Snack.Handle.Render
  ( renderSWidget,
  )
where

import Brick.AttrMap (attrName)
import qualified Brick.Types as T
import Brick.Widgets.Core
import Snack.CommandLine
import Snack.Completion.Event (CompletionFunc)
import Snack.Completion.State
import Snack.SnackWidget

renderCommandLine ::
  (Ord n, Show n, Monoid t, TextWidth t, GenericCommandLineEditor t) =>
  ([t] -> T.Widget n) ->
  CommandLine t n ->
  t ->
  T.Widget n
renderCommandLine draw cl completion =
  let cursorLoc = T.Location (textWidth (toLeft cl), cursorRowContents cl)
      name = commandLineName cl
      atChar = charAtCursor cl
      atCharWidth = maybe 1 textWidth atChar
      contents = getCommandLineContents cl
   in showCursor name cursorLoc $
        visibleRegion cursorLoc (atCharWidth, 1) $
          draw $
            contents ++ [getCurrentLine cl <> completion]

renderSWidget :: (Ord n, Show n) => SnackWidget n -> T.Widget n
renderSWidget (SnackWidget n _ commandL completionS _) =
  reportExtent n $ viewport n T.Vertical $ renderedCommandLine
  where
    renderedCommandLine = renderCommandLine ((<=> completionBar) . str . unlines) commandL selectedInsert

    completionBar =
      padLeft ((T.Pad . length . toLeft) commandL) . withDefAttr (attrName "completionBar") $
        vBox
          ( map (withDefAttr (attrName "completionBar") . str) before
              ++ (withDefAttr (attrName "selectedCompletion") . str) selected :
            map (withDefAttr (attrName "completionBar") . str) after
          )
    (selected, before, after) = completionBox completionS
    (selectedInsert, _, _) = completionInsert completionS

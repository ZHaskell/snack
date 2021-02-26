module Snack.Widget.Render (
  newWidget,
  renderSWidget
) where

import Brick.AttrMap (attrName)
import qualified Brick.Types as T
import Brick.Widgets.Core
import qualified Brick.Widgets.Edit as E
import Snack.Event.Completion (CompletionFunc)
import Snack.Widget.Editor (toLeft)
import Snack.Widget.Type

newWidget :: n -> n -> CompletionFunc IO -> SnackWidget n
newWidget n m complete =
  MkSnackWidget
    { name = n,
      extent = Nothing,
      snackEditor = E.editor m Nothing ("" :: String),
      snackEditor' = E.editor m Nothing ("" :: String),
      completionBox = ("", [], []),
      completionInsert = ("", [], []),
      completionFunc = complete,
      isInCompletion = False
    }

renderSWidget :: (Ord n, Show n) => SnackWidget n -> T.Widget n
renderSWidget (MkSnackWidget n _ se _ (selected, before, after) _ _ _) =
  reportExtent n $ viewport n T.Vertical $ hLimit 80 (vLimit 20 renderedEditor)
  where
    renderedEditor = E.renderEditor ((<=> completionBar) . str . unlines) True se

    completionBar =
      padLeft ((T.Pad . length . toLeft) se) . withDefAttr (attrName "completionBar") $
        vBox
          ( map (withDefAttr (attrName "completionBar") . str) before
              ++ (withDefAttr (attrName "selectedCompletion") . str) selected :
            map (withDefAttr (attrName "completionBar") . str) after
          )

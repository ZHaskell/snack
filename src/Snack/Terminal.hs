module Snack.Terminal(
  runSnackWithDefaultSetting
) where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util
import Control.Monad (void)
import qualified Graphics.Vty as V
import Snack.Event.Completion
import Snack.Event.HandleEvent
import Snack.Widget.Render
import Snack.Widget.Type

runSnackWithDefaultSetting :: (String -> IO [Completion]) -> IO ()
runSnackWithDefaultSetting complete = do
  let builder = V.mkVty V.defaultConfig
      completef = completeWord (Just '\\') " " complete
  initialVty <- builder
  void $ M.customMain initialVty builder Nothing app (initialState completef)

data Name = Terminal | Editor deriving (Eq, Ord, Show)

initialState :: CompletionFunc IO -> SnackWidget Name
initialState = newWidget Terminal Editor

app :: M.App (SnackWidget Name) V.Event Name
app =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = \_ -> M.showCursorNamed Editor,
      M.appHandleEvent = handleBrickEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (A.attrName "completionBar", V.blue `on` V.white),
      (A.attrName "selectedCompletion", V.red `on` V.white)
    ]

drawUI :: (Show n, Ord n) => SnackWidget n -> [T.Widget n]
drawUI sw = [renderSWidget sw]

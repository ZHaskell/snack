module Snack.Main
  ( runSnackWithDefaultSetting,
    getCommand,
    outPrintLn,
    loop,
    CommandT
  )
where

import qualified Brick.AttrMap              as A
import           Brick.BChan                (BChan, newBChan, readBChan,
                                             writeBChan)
import qualified Brick.Main                 as M
import qualified Brick.Types                as T
import           Brick.Util                 (on)
import           Control.Concurrent         (forkIO)
import           Control.Monad              (void)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import qualified Graphics.Vty               as V
import           Snack.AppEvent             (TerminalEvent (PrintLn))
import           Snack.Completion           (commandOptionCompletion)
import           Snack.Handle.Event         (handleBrickEvent)
import           Snack.Handle.Render        (renderSWidget)
import           Snack.Parser               (Parser)
import           Snack.SnackWidget          (SnackSettings (Settings),
                                             SnackWidget, newWidget)

--------------------------------
runSnackWithDefaultSetting ::
    CommandT b a
    -> [(String, Parser b)]
    -> IO ()
runSnackWithDefaultSetting commandT commandsL = do
  requestChan <- newBChan 1
  readChan <- newBChan 1
  let builder = V.mkVty V.defaultConfig
      completionF = commandOptionCompletion commandsL
      settings = Settings Terminal commandsL readChan completionF
  initialVty <- builder
  void $ forkIO (runCommandT (readChan, requestChan) commandT)
  void $ M.customMain initialVty builder (Just requestChan) app (initialState settings)

data Name = Terminal | Editor deriving (Eq, Ord, Show)

---------------------------------------------------
type ReadChan b = BChan b
type RequestChan = BChan TerminalEvent
type CommandT b a = ReaderT (ReadChan b, RequestChan) IO a

runCommandT :: (ReadChan b, RequestChan) -> CommandT b a -> IO ()
runCommandT chanTuple = void . flip runReaderT chanTuple

getCommand :: CommandT b b
getCommand = do
    (chan, _) <- ask
    liftIO $ readBChan chan

outPrintLn :: String -> CommandT b ()
outPrintLn s = do
    (_, chan) <- ask
    liftIO $ writeBChan chan (PrintLn s)

loop :: CommandT b a -> CommandT b a
loop commandT = commandT >> loop commandT

--------------------------------
-- defaultSetting :: SnackSettings Name a
-- defaultSetting = Settings {
--   widgetName = Terminal,
--   commandsList = []
--   }

initialState :: SnackSettings Name a -> SnackWidget Name a
initialState = newWidget Editor

app :: M.App (SnackWidget Name a) TerminalEvent Name
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

drawUI :: (Show n, Ord n) => SnackWidget n a -> [T.Widget n]
drawUI sw = [renderSWidget sw]

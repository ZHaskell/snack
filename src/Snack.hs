{-# LANGUAGE LambdaCase #-}

module Snack where 
    
import System.Console.Haskeline
import System.Console.Haskeline.Prefs
import Transform

import Brick
import Brick.BChan
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Graphics.Vty hiding (Event)

import Control.Monad (void)
import Control.Concurrent (forkFinally)
import Control.Exception
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.List (isPrefixOf)

data Event = FromHBWidget HSLineEvent
           | HaskelineDied (Either SomeException ())

data Name = TheApp | HaskelineWidget
    deriving (Ord, Eq, Show)

newtype WidgetState = WidgetState { getHWidget :: HWidget Name }

initialState :: WidgetState
initialState = WidgetState (initialWidget HaskelineWidget)

app :: TSConfig Event -> App WidgetState Event Name
app config = App { appDraw = drawUI
                 , appChooseCursor = \_ -> showCursorNamed HaskelineWidget
                 , appHandleEvent = handleEvent config
                 , appStartEvent = return
                 , appAttrMap = const theMap
                 }

handleEvent :: TSConfig Event
            -> WidgetState -> BrickEvent Name Event -> EventM Name (Next WidgetState)
handleEvent config (WidgetState hw) event = do
    hw' <- handleBrickEvent config hw event
    isTerminated (WidgetState hw') event

isTerminated :: WidgetState -> BrickEvent Name Event -> EventM Name (Next WidgetState)
isTerminated s (AppEvent (HaskelineDied e)) = halt s
isTerminated s (VtyEvent (EvKey KEsc [])) = halt s
isTerminated s _ = continue s

drawUI :: WidgetState -> [Widget Name]
drawUI s = [B.border $ renderHWidget (getHWidget s)]

theMap :: AttrMap
theMap = attrMap defAttr [(attrName "completionBar", blue `on` white),
                          (attrName "selectedCompletion", red `on` white)]


settings :: MonadIO m => Settings m 
settings = setComplete (completeWord Nothing [] completeSnacks) defaultSettings

snackList = ["kitkat", "twirst", "mars", "sweets", "crisps", "doritos", "pretzels", "peanuts"]


completeSnacks :: MonadIO m => String -> m [Completion]
completeSnacks str = return . map simpleCompletion . filter (str `isPrefixOf`) $ snackList 


runHaskeline :: TSConfig Event -> IO ()
runHaskeline config = runInputTBehaviorWithPrefs (brickBehavior config) defaultPrefs {completionType = MenuCompletion} settings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "snack> "
            case minput of
                Nothing    -> return ()
                Just "quit"-> return ()
                Just input -> do
                    outputStr input
                    loop

main :: IO ()
main = do
    chan <- newBChan 10
    config <- configure chan FromHBWidget (\case { FromHBWidget x -> Just x; _ -> Nothing })
    -- runHaskeline config
    _ <- forkFinally (runHaskeline config) (writeBChan chan . HaskelineDied)
    let builder = mkVty defaultConfig
    initialVty <- builder
    void $ customMain initialVty builder (Just chan) (app config) initialState
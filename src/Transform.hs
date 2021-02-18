module Transform where 

import System.Console.Haskeline.Term
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads
import qualified System.Console.Haskeline.InputT as I
import qualified System.Console.Haskeline.Key as K

-- import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Catch ( MonadMask, MonadCatch, MonadThrow )
import Brick 
import qualified Brick.BChan as BC
import qualified Graphics.Vty as V
import Data.List.Split

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent.MVar

data TSConfig e = MkTSConfig { fromBrickChan :: TChan Event  -- Haskeline Term Event 
                             , toBrickChan :: BC.BChan e   
                             , toBrickEvent :: HSLineEvent-> e
                             , fromBrickEvent :: e -> Maybe HSLineEvent
                             }

data HSLineEvent = LayoutRequest (MVar (Maybe Layout))
                 | MoveToNextLine
                 | PrintLines [String]
                 | DrawLineDiff LineChars
                 | ClearLayout
                 | CompletionSelections (String, [String])

configure :: BC.BChan e
          -> (HSLineEvent -> e)
          -> (e -> Maybe HSLineEvent)
          -> IO (TSConfig e)
configure toBrickChan' toBrickEvent' fromBrickEvent' = do
    chan <- atomically newTChan
    return $ MkTSConfig { fromBrickChan = chan
                        , toBrickChan = toBrickChan'
                        , toBrickEvent = toBrickEvent'
                        , fromBrickEvent = fromBrickEvent'
                        }

initialWidget :: n -> HWidget n
initialWidget n = MkHWidget { name = n
                            , visibleLines = []
                            , hiddenLines = []
                            , current = ("", "")
                            , extent = Nothing
                            , completionBox = ("",[])
                            }
------------------------------------------------------------------------
-- Brick -> HWidget  
data HWidget n = MkHWidget { name :: n
                           , visibleLines :: [String]
                           , hiddenLines :: [String]
                           , current :: (String, String)
                           , extent :: Maybe (Int, Int)
                           , completionBox :: (String, [String])
                           }
                            
handleBrickEvent :: (Eq n) => TSConfig e -> HWidget n -> BrickEvent n e -> EventM n (HWidget n)
handleBrickEvent config hwidget (AppEvent event) =
    case (fromBrickEvent config) event of
        Just (LayoutRequest mvar) -> do
            extent <- lookupExtent (name hwidget)
            case extent of
                Just (Extent _ _ (width, height)) -> do
                    liftIO . putMVar mvar . Just $ Layout width height
                    return hwidget { extent = Just (width, height) }
                Nothing -> do
                    liftIO . putMVar mvar $ Nothing
                    return hwidget

        Just MoveToNextLine -> do
            let (pre,suff) = current hwidget
                hwidget'   = hwidget { visibleLines = visibleLines hwidget ++ [pre ++ suff]
                                     , current     = ("", "")
                                     }
                vp         = viewportScroll (name hwidget)
            vScrollToEnd vp
            return hwidget'

        Just (PrintLines ls) -> return hwidget { visibleLines = visibleLines hwidget ++ ls }

        Just (DrawLineDiff (pre, suff)) -> return hwidget { current = ( graphemesToString pre
                                                                      , graphemesToString suff) }
      
        Just ClearLayout -> return hwidget { visibleLines = []
                                           , hiddenLines = hiddenLines hwidget ++ visibleLines hwidget
                                           }
      
        Nothing -> return hwidget

        Just (CompletionSelections s) -> return hwidget { completionBox = s }

handleBrickEvent config hwidget (VtyEvent (V.EvKey k mods)) = do
    liftIO . atomically . writeTChan (fromBrickChan config) $ mkKeyEvent k
    return hwidget
    where
        mkKeyEvent :: V.Key -> Event
        mkKeyEvent key =  
            case key of 
                V.KChar '\t' -> keyToEvent (K.KeyChar '\t') 
                V.KChar char -> keyToEvent' (K.KeyChar char) 
                V.KEnter     -> keyToEvent (K.KeyChar '\n') 
                V.KBS        -> keyToEvent' K.Backspace 
                V.KDel       -> keyToEvent' K.Delete 
                V.KLeft      -> keyToEvent' K.LeftKey 
                V.KRight     -> keyToEvent' K.RightKey 
                V.KUp        -> keyToEvent K.UpKey 
                V.KDown      -> keyToEvent K.DownKey 
                _            -> KeyInput []
        
        keyToEvent = KeyInput . (:[]) . addModifiers mods . K.simpleKey
        keyToEvent' = KeyInput . (:[tab]) . addModifiers mods . K.simpleKey
        tab = addModifiers [] . K.simpleKey . K.KeyChar $ '\t' 

        addModifiers :: [V.Modifier] -> K.Key -> K.Key
        addModifiers []     key = key 
        addModifiers (m:ms) key = addModifiers ms $
            case m of 
                V.MAlt   -> key
                V.MMeta  -> K.metaKey key
                V.MShift -> case key of  
                                K.Key mod baseKey -> K.Key mod { K.hasShift = True } baseKey
                V.MCtrl  -> case key of 
                                K.Key mod (K.KeyChar char) -> let K.Key _ baseKey = K.ctrlChar char in K.Key mod baseKey
                                _                          -> K.ctrlKey key

handleBrickEvent _ hwidget (VtyEvent (V.EvResize _ _)) = do
    me <- lookupExtent (name hwidget)
    case me of
        Just (Extent _ _ (width, height)) -> return hwidget { extent = Just (width, height) }
        Nothing                           -> return hwidget

handleBrickEvent _ hwidget _ = return hwidget


-------
-- brick Haskeline backend
brickBehavior :: TSConfig e -> I.Behavior
brickBehavior config = I.Behavior (brickRunTerm config)

brickRunTerm :: TSConfig e -> IO RunTerm
brickRunTerm config = return RunTerm 
                                 { putStrOut = putStrOutBrickEvent
                                 , termOps = Left tops
                                 , wrapInterrupt = id
                                 , closeTerm = return ()
                                 }
    where 
        putStrOutBrickEvent :: String -> IO ()
        putStrOutBrickEvent s = BC.writeBChan (toBrickChan config) . toBrickEvent config . PrintLines $ [s]

        getLayoutBrickEvent :: IO Layout
        getLayoutBrickEvent = do
            mvar <- newEmptyMVar
            let event = toBrickEvent config (LayoutRequest mvar)
            BC.writeBChan (toBrickChan config) event
            layout <- takeMVar mvar
            return $ case layout of
                Just l  -> l
                Nothing -> Layout 0 0

        withGetEventBrick :: forall m a . CommandMonad m
                      => (m Event -> m a) -> m a
        withGetEventBrick f = f . liftIO . atomically . readTChan $ (fromBrickChan config)

        tops = TermOps { getLayout = getLayoutBrickEvent
                       , withGetEvent = withGetEventBrick
                       , saveUnusedKeys = saveKeys (fromBrickChan config)
                       , evalTerm = evalBrickTerm config
                       , externalPrint = atomically . writeTChan (fromBrickChan config) . ExternalPrint
                       }

------------------------------------------------------------------------
-- Haskeline -> Brick

             
newtype BrickTerm m a = MkBrickTerm { unBrickTerm :: ReaderT (HSLineEvent -> IO ()) m a }
    deriving ( MonadIO, Monad, MonadThrow, MonadCatch, MonadMask, Applicative, Functor, MonadReader (HSLineEvent -> IO ()) )

instance MonadTrans BrickTerm where
    lift = MkBrickTerm . lift

evalBrickTerm :: (CommandMonad m) => TSConfig e -> EvalTerm m
evalBrickTerm config = EvalTerm (runReaderT' send . unBrickTerm) lift --(MkBrickTerm . lift)
    where 
        send = BC.writeBChan (toBrickChan config) . toBrickEvent config

instance (MonadReader Layout m, MonadMask m, MonadIO m) => Term (BrickTerm m) where
    reposition _ _   = pure ()
    moveToNextLine _ = sendToBrick MoveToNextLine
    printLines ls    = sendToBrick $ PrintLines ls
    drawLineDiff _ d = sendToBrick $ DrawLineDiff d
    clearLayout      = sendToBrick $ ClearLayout
    ringBell _       = pure ()
    completionSelections s = sendToBrick $ CompletionSelections s

sendToBrick :: MonadIO m => HSLineEvent -> BrickTerm m ()
sendToBrick e = ask >>= (\f -> liftIO (f e))

renderHWidget :: (Ord n, Show n) => HWidget n -> Widget n
renderHWidget (MkHWidget name lines _ (pre, suff) extent (selected, selections)) =
    reportExtent name $ viewport name Vertical $ prev <=> curr <=> completionBar
        where
            
            prev = vBox . map str . concat . map (wrap extent) $ lines
            curr = visible . showCursor name (Location (location extent)) . vBox . map str . wrap extent $ pre ++ suff
            
            completionBar = padLeft ((Pad . fst . location) extent) . withDefAttr (attrName "completionBar") $ vBox ((withDefAttr (attrName "selectedCompletion") . str) selected : 
                            map (withDefAttr (attrName "completionBar") . str) selections)

            location Nothing       = (length pre, 0)
            location (Just (w, _)) = let (q, r) = divMod (length pre) w 
                                     in  (r, q)

            wrap :: Maybe (Int, Int) -> String -> [String]
            wrap Nothing       l = [l]
            wrap (Just (w, _)) l = chunksOf w l
           
            -- name        = name hwidget
            -- (pre, suff) = current hwidget
            -- lines       = visibleLines hwidget
            -- extent      = extent hwidget

------

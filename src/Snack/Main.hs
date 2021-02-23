module Snack.Main where 

import qualified Graphics.Vty         as V

import qualified Brick.Main           as M
import qualified Brick.Types          as T
-- import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit   as E
import qualified Brick.AttrMap        as A
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
    ( (<=>),
      hLimit,
      padLeft,
      reportExtent,
      str,
      vBox,
      vLimit,
      viewport,
      withDefAttr )
import Brick.Util (on)

import Control.Monad.IO.Class
import Control.Monad (void)
import Data.List (isPrefixOf)
-- import Data.List.Split
import Snack.Completion

import qualified Snack.Editor as SE

data Name = Terminal1 | Editor1 deriving (Eq, Ord, Show)
data LineEvent = PrintLines [String]
               | ClearLayout
               | CompletionSelections (String, [String])

data SnackWidget n = MkSnackWidget
    { name          :: n
    , visibleLines  :: [String]
    , hiddenLines   :: [String]
    , current       :: (String, String)
    , extent        :: Maybe (Int, Int)
    , snackEditor   :: E.Editor String n
    , snackEditor'  :: E.Editor String n
    , completionBox :: (String, [String], [String])
    , completionInsert :: (String, [String], [String])
    , completionFunc :: CompletionFunc IO
    , isInCompletion :: Bool
    } 

initialWidget :: n -> n -> SnackWidget n 
initialWidget n m = MkSnackWidget 
    { name          = n
    , visibleLines  = []
    , hiddenLines   = []
    , current       = ("", "")
    , extent        = Nothing
    , snackEditor   = E.editor m Nothing ("" :: String)
    , completionBox = ("",[],[])
    , completionInsert = ("",[],[])
    , completionFunc = completeWord (Just '\\') " " completeSnacks
    , isInCompletion = False 
    }
------------------------------------------------------------------------
            
handleBrickEvent :: (Eq n) => SnackWidget n -> T.BrickEvent n e -> T.EventM n (T.Next (SnackWidget n))
handleBrickEvent swidget (T.VtyEvent event) = do
    case event of 
        V.EvKey V.KEsc []         
            | isInCompletion swidget -> M.continue swidget {snackEditor = snackEditor' swidget, isInCompletion = False }
            | otherwise              -> M.halt swidget
        V.EvKey (V.KChar '\t') [] 
            | isInCompletion swidget -> do 
                let (x, _, _)      = completionInsert swidget
                    newSnackEditor = SE.insertMany x (snackEditor' swidget)
                M.continue swidget {completionBox    = cycleCompletion (completionBox swidget),
                                    completionInsert = cycleCompletion (completionInsert swidget),
                                    snackEditor      = newSnackEditor}
            | otherwise -> do 
                (_, completions) <- liftIO $ completionFunc swidget ( reverse $ (SE.toLeft . snackEditor) swidget
                                                                              , (SE.toRight . snackEditor) swidget)
                if null completions 
                    then M.continue swidget                       
                    else M.continue swidget { completionBox    = presentCompletion completions,
                                              completionInsert = getCompletionReplacement completions,
                                              isInCompletion   = True }
        
        _                         -> do editor <- E.handleEditorEvent event (snackEditor swidget)
                                        M.continue $ setStateFromEditor swidget { snackEditor = editor, snackEditor' = editor, completionBox = ("",[],[]), isInCompletion = False }

handleBrickEvent swidget _ = M.continue swidget
-- handleBrickEvent _ swidget (VtyEvent (V.EvResize _ _)) = do
--     me <- lookupExtent (name swidget)
--     case me of
--         Just (Extent _ _ (width, height)) -> return swidget { extent = Just (width, height) }
--         Nothing                           -> return swidget
--

renderSWidget :: (Ord n, Show n) => SnackWidget n -> T.Widget n
renderSWidget (MkSnackWidget n _ _ (pre, _) e se _ (selected, before, after)_ _ _) =
    reportExtent n $ viewport n T.Vertical $ hLimit 80 (vLimit 20 renderedEditor) 
    where
        renderedEditor = E.renderEditor ((<=> completionBar) . str . unlines) True se
        -- prev = vBox . map str . concat . map (wrap extent) $ lines
        -- curr = visible . showCursor n (T.Location (location e)) . vBox . map str . wrap extent $ pre ++ suff
            
        completionBar = padLeft ((T.Pad . fst . location) e) . withDefAttr (A.attrName "completionBar") 
                      $ vBox (map (withDefAttr (A.attrName "completionBar") . str) before
                      ++ (withDefAttr (A.attrName "selectedCompletion") . str) selected 
                      : map (withDefAttr (A.attrName "completionBar") . str) after)
        
        location Nothing       = (length pre, 0)
        location (Just (w, _)) = let (q, r) = divMod (length pre) w 
                                 in  (r, q)

        -- wrap :: Maybe (Int, Int) -> String -> [String]
        -- wrap Nothing       l = [l]
        -- wrap (Just (w, _)) l = chunksOf w l
            
---------------------------------------------------------

initialState :: SnackWidget Name
initialState = initialWidget Terminal1 Editor1

app :: M.App (SnackWidget Name) V.Event Name
app = M.App { M.appDraw = drawUI
            , M.appChooseCursor = \_ -> M.showCursorNamed Editor1
            , M.appHandleEvent = handleBrickEvent
            , M.appStartEvent = return
            , M.appAttrMap = const theMap
            }

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr [(A.attrName "completionBar",      V.blue `on` V.white),
                              (A.attrName "selectedCompletion", V.red  `on` V.white)]

drawUI :: SnackWidget Name -> [T.Widget Name]
drawUI sw = [B.border $ renderSWidget sw]

runSnackWithSetting :: IO ()
runSnackWithSetting = do
    -- chan <- newBChan 10
    let builder = V.mkVty V.defaultConfig
    initialVty <- builder
    void $ M.customMain initialVty builder Nothing app initialState


--helper functions-----------------
setStateFromEditor :: SnackWidget n -> SnackWidget n
setStateFromEditor swidget = swidget 
    { visibleLines = visibleLines swidget ++ lines (SE.above editor)
    , current      = (SE.toLeft editor, SE.toRight editor)}
    where 
        editor = snackEditor swidget

cycleCompletion :: (a, [a], [a]) -> (a, [a], [a])
cycleCompletion (x, [], []) = (x, [], [])
cycleCompletion (x, y:ys, []) = (y, [], ys ++ [x])
cycleCompletion (x, [], z:zs) = (z, [x], zs)
cycleCompletion (x, y:ys, z:zs) = (z, y:ys++[x], zs) 

-------------------
 
snackList :: [[Char]]
snackList = ["kitkat", "twirst", "mars", "sweets", "crisps", "doritos", "pretzels", "peanuts"]

completeSnacks :: MonadIO m => String -> m [Completion]
completeSnacks string = return . map complete . filter (string `isPrefixOf`) $ snackList 
   where 
       complete x = setReplacement (drop (length string)) (simpleCompletion x)


module Main where

import           Data.List              (isPrefixOf)
import           Snack

data Commands = Picnic PicnicArgs
              | Break BreakArgs

data PicnicArgs = PicnicArgs {
  snackPicnic      :: String,
  hunguryPicnic    :: Bool,
  enthusiasmPicnic :: Int
} deriving Show

data BreakArgs = BreakArgs {
  snackBreak :: String,
  hunguryBreak :: Bool,
  enthusiasmBeak :: Int
} deriving Show

picnic :: Parser Commands
picnic = Picnic <$>
       (PicnicArgs <$> strOption
         ( long "Snack"
         <> metavar "SNACK"
         <> help "What snack to have"
         <> completeWith snackList )
      <*> switch
          ( long "hungury"
         <> short 'h'
         <> help "Whether hungury to eat" )
      <*> option auto
          ( long "enthusiasm"
         <> short 'e'
         <> help "How enthusiastically to greet"
         <> value 1
         <> metavar "INT" ))

commandsList :: [(String, Parser Commands)]
commandsList = [("PICNIC", picnic)]

snackList :: [String]
snackList = ["kitkat", "twirst", "mars", "sweets", "crisps", "doritos", "pretzels", "peanuts"]

snackAnswer :: Commands -> CommandT Commands ()
snackAnswer (Picnic (PicnicArgs snack True n))
  | snack `elem` snackList = outPrintLn ("Yes please" ++ replicate n '!' ++ " " ++ snack ++ " will be great" ++ replicate n '!')
  | otherwise = return ()
snackAnswer _ = return ()

programSnack :: CommandT Commands ()
programSnack = loop $ getCommand >>= snackAnswer

main :: IO ()
main =
  runSnackWithDefaultSetting programSnack commandsList

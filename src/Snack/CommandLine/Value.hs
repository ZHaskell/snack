{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Snack.CommandLine.Value where

import qualified Data.ByteString           as BS
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Snack.CommandLine.Editor  as E
import qualified Snack.CommandLine.Generic as E

data CommandLine t n = CommandLine
  { commandLineInput    :: E.CommandLineEditor t,
    -- | all of the command line contents, except the current line
    commandLineContents :: [t],
    commandLineHistory  :: [t],
    commandLineName     :: n
  }

instance (Show t, Show n) => Show (CommandLine t n) where
  show e =
    concat
      [ "CommandLine { ",
        "CommandLineInput = " <> show (commandLineContents e),
        ", CommandLineContents = " <> show (commandLineContents e),
        ", CommandLineHistory = " <> show (commandLineHistory e),
        ", CommandLineName = " <> show (commandLineName e),
        "}"
      ]

instance (Eq t) => Eq (CommandLine t n) where
  CommandLine a b c _ == CommandLine d e f _ = a == d && b == e && c == f

-- | Values that can be constructed by decoding bytestrings in UTF-8
-- encoding.
class DecodeUtf8 t where
  -- | Decode a bytestring assumed to be text in UTF-8 encoding. If
  -- the decoding fails, return 'Left'. This must not raise
  -- exceptions.
  decodeUtf8 :: BS.ByteString -> Either String t

instance DecodeUtf8 T.Text where
  decodeUtf8 bs = case T.decodeUtf8' bs of
    Left e  -> Left $ show e
    Right t -> Right t

instance DecodeUtf8 String where
  decodeUtf8 bs = T.unpack <$> decodeUtf8 bs

-- | initialize a command line
initialCommandLine :: E.GenericCommandLineEditor t => n -> CommandLine t n
initialCommandLine = CommandLine (E.mkEditor []) mempty mempty

-- cursorPosition :: CommandLine t n -> (Int, Int)

-- | Get the column number of the cursor
cursorCol :: E.GenericCommandLineEditor t => CommandLine t n -> Int
cursorCol = E.length . toLeft

-- | Get the row number of the cursor
cursorRowContents :: CommandLine t n -> Int
cursorRowContents = length . commandLineContents

cursorRowInput :: CommandLine t n -> Int
cursorRowInput = length . above

-- | Get the contSfdccent from the beginning of line to the left of cursor
toLeft :: CommandLine t n -> t
toLeft = E.toLeft . commandLineInput

-- | Get the content from the right of cursor to the end of line
toRight :: CommandLine t n -> t
toRight = E.toRight . commandLineInput

above :: CommandLine t n -> [t]
above = E.above . commandLineInput

below :: CommandLine t n -> [t]
below = E.below . commandLineInput

-- | Get all the history of CommandLine Input
getCommandLineInput :: Monoid t => CommandLine t n -> [t]
getCommandLineInput = E.getText . commandLineInput

-- | Get the current command line input
getCurrentLine :: Monoid t => CommandLine t n -> t
getCurrentLine = E.currentLine . commandLineInput

-- | get the whole command line contents, except the current line
getCommandLineContents :: Monoid t => CommandLine t n -> [t]
getCommandLineContents = commandLineContents

charAtCursor :: E.GenericCommandLineEditor t => CommandLine t n -> Maybe t
charAtCursor cl =
  if E.length (toRight cl) > 0
    then Just $ E.take 1 (toRight cl)
    else Nothing

isLastLine :: CommandLine t n -> Bool
isLastLine = null . below

isFirstLine :: CommandLine t n -> Bool
isFirstLine = null . above

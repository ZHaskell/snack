{-# LANGUAGE OverloadedStrings #-}

module Snack.CommandLine.Editor where

import Prelude hiding (drop, init, last, length, lines, null, take)
import qualified Prelude

data CommandLineEditor a = CLE
  { toLeft :: a,
    toRight :: a,
    above :: [a],
    below :: [a]
  }
  deriving (Eq, Show)

-- | Create a zipper using a custom text storage type. Takes the initial
-- text as well as all of the functions necessary to manipulate the
-- underlying text values.
mkEditor ::
  Monoid a =>
  [a] ->
  CommandLineEditor a
mkEditor [] = CLE mempty mempty mempty mempty
mkEditor ls = CLE (Prelude.last ls) mempty (Prelude.init ls) mempty

mkEditorFromHistory ::
  Monoid a =>
  [a] ->
  CommandLineEditor a
mkEditorFromHistory ls = CLE mempty mempty ls mempty

-- | Get the text contents of the zipper.
getText :: Monoid t => CommandLineEditor t -> [t]
getText tz = above tz ++ (currentLine tz : below tz)

currentLine :: Monoid t => CommandLineEditor t -> t
currentLine tz = (toLeft tz) <> (toRight tz)

{-# LANGUAGE FlexibleInstances #-}

module Snack.CommandLine.Generic where

import qualified Data.Text as T
import           Prelude   hiding (drop, init, last, length, lines, null, take,
                            words)
import qualified Prelude

class Monoid a => GenericCommandLineEditor a where
  singleton :: Char -> a
  drop :: Int -> a -> a
  take :: Int -> a -> a
  length :: a -> Int
  last :: a -> Char
  init :: a -> a
  null :: a -> Bool
  lines :: a -> [a]
  words :: a -> [a]
  toList :: a -> [Char]

instance GenericCommandLineEditor [Char] where
  singleton = (: [])
  drop = Prelude.drop
  take = Prelude.take
  length = Prelude.length
  last = Prelude.last
  init = Prelude.init
  null = Prelude.null
  lines = Prelude.lines
  words = Prelude.words
  toList = id

instance GenericCommandLineEditor T.Text where
  singleton = T.singleton
  drop = T.drop
  take = T.take
  length = T.length
  last = T.last
  init = T.init
  null = T.null
  lines = T.lines
  words = T.words
  toList = T.unpack

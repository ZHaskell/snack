{-# LANGUAGE OverloadedStrings #-}
module Snack.Handle.Attribute where

import           Brick.AttrMap           (AttrMap, AttrName, attrMap)
import           Brick.Util              (fg, on)
import           Graphics.Vty.Attributes (Attr, Color, black, defAttr, red,
                                          rgbColor)

commandColors :: AttrMap
commandColors = attrMap defAttr attrList

attrList :: [(AttrName, Attr)]
attrList = [("empty", commandNotExist)]

commandNotExist :: Attr
commandNotExist = black `on` red

helpCommand :: Attr
helpCommand = fg emeraldGreen

exitCommand :: Attr
exitCommand = fg fireRed

normalCommand :: Attr
normalCommand = fg vividTangerine

fireRed :: Color
fireRed = rgbColor (238 :: Integer) 99 82

vividTangerine :: Color
vividTangerine = rgbColor (247 :: Integer) 157 132

emeraldGreen :: Color
emeraldGreen = rgbColor (89 :: Integer) 205 144

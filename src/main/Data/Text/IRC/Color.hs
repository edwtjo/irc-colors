-- |
-- Module : Data.Text.IRC.Color
-- Copyright : (c) Edward Tjörnhammar 2015
-- License : BSD3
--
-- Maintainer : ed@cflags.cc
-- Stability : Alpha
-- Portability : Portable
--
-- Small Text colourizer for IRC messages
--
module Data.Text.IRC.Color (
  -- Decorator renderers
  style
  , fg
  , bg
  , fgBg
  , rainbow
  , white
  , black
  , navy
  , green
  , red
  , brown
  , purple
  , olive
  , yellow
  , lime
  , teal
  , cyan
  , blue
  , pink
  , grey
  , silver
  -- Styles
  , bold
  , italic
  , underline
  , normal
) where

import Data.Char (chr,ord)
import Data.Monoid
import Data.Text hiding (map, zip, length)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder

type Code = String

data Color = MkColor Code
data Style = MkStyle Code

type Decoration = (Maybe Color, Maybe Color)

-- |Colors known to work on common IRC clients, constructs a Color
white, black, navy, green, red, brown, purple, olive, yellow, lime, teal, cyan, blue, pink, grey, silver :: Color
white = MkColor "0"
black = MkColor "1"
navy  = MkColor "2"
green = MkColor "3"
red   = MkColor "4"
brown = MkColor "5"
purple = MkColor "6"
olive  = MkColor "7"
yellow = MkColor "8"
lime = MkColor "9"
teal = MkColor "10"
cyan = MkColor "11"
blue = MkColor "12"
pink = MkColor "13"
grey = MkColor "14"
silver = MkColor "15"

-- |Styles known to work on common IRC clients, constructs a Style.
bold, italic, underline, normal :: Style
bold = MkStyle [chr 2]
italic = MkStyle [chr 22]
underline = MkStyle [chr 31]
normal = MkStyle [chr 15]

-- Non exported helpers
mark = fromString [chr 3]

withMark :: Text -> Builder -> Builder
withMark txt b = mark <> b <> fromText txt <> mark

toText :: Builder -> Text
toText = LT.toStrict.toLazyText

color :: Decoration -> Text -> Text
color (Just (MkColor fg), Just (MkColor bg)) txt  = toText $ withMark txt $ fromString $ fg ++ "," ++ bg
color (Nothing, Just (MkColor bg)) txt            = toText $ withMark txt $ fromString $ "," ++ bg
color (Just (MkColor fg), Nothing) txt            = toText $ withMark txt $ fromString fg
color (Nothing, Nothing) txt                      = toText $ withMark txt mempty

-- |Foreground colourizer
fg :: Color -> Text -> Text
fg c = color (Just c, Nothing)

-- |Background colourizer
bg :: Color -> Text -> Text
bg c = color (Nothing, Just c)

-- |Apply both a foreground and a background color to a Text
fgBg :: Color -> Color -> Text -> Text
fgBg f b = color (Just f, Just b)

-- |Apply a style to a Text
style :: Style -> Text -> Text
style (MkStyle s) txt = toText $ withMark txt $ fromString s

-- |Inefficiently turn a Text into a rainbow of joy
rainbow :: Text -> Text
rainbow = asRainbow
  where
    cntChrs t = zip [(ord $ T.head t)..] $ chunksOf 1 t
    colors = [ red, olive, yellow, green, blue, navy, purple ]
    lenC = length colors
    colour i = colors !! (i `mod` lenC)
    asRainbow t = T.concat $ map (\(i,t) -> fg (colour i) t) $ cntChrs t

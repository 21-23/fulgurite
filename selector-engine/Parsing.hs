module Parsing
  ( Parser
  , tryOrEmpty
  , d
  , e
  , n
  , o
  , t
  , v
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, option, try)

type Parser = Parsec Void Text

tryOrEmpty :: Parser Text -> Parser Text
tryOrEmpty p = option "" $ try p

{-
  D         d|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?
  E         e|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?
  N         n|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n
  O         o|\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\o
  T         t|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t
  V         v|\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\v
-}

d :: Parser Text
d = "d"

e :: Parser Text
e = "e"

n :: Parser Text
n = tryOrEmpty "\\" <> "n"

o :: Parser Text
o = tryOrEmpty "\\" <> "o"

t :: Parser Text
t = tryOrEmpty "\\" <> "t"

v :: Parser Text
v = tryOrEmpty "\\" <> "v"

module Protocol.Game
  ( Game(..)
  , stringify
  , parseGame
  ) where

import Control.Applicative (Alternative(empty))
import Data.Text (Text)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), Value(String))

data Game
  = CSSQuickDraw
  | LodashQuickDraw
  | JSQuickDraw
  deriving stock (Eq, Read, Show, Ord)

stringify :: Game -> Text
stringify CSSQuickDraw    = "cssqd"
stringify LodashQuickDraw = "_qd"
stringify JSQuickDraw     = "jsqd"

parseGame :: Alternative m => Text -> m Game
parseGame "cssqd" = pure CSSQuickDraw
parseGame "_qd"   = pure LodashQuickDraw
parseGame "jsqd"  = pure JSQuickDraw
parseGame _       = empty

instance ToJSON Game where
  toJSON = String . stringify

instance FromJSON Game where
  parseJSON (String string) = parseGame string
  parseJSON value           = fail $ "Unrecognized game: " <> show value

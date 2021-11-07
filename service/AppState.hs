module AppState
  ( AppState(..)
  , PuzzleData(..)
  ) where

import Data.Text (Text)
import Element (Element)

data PuzzleData = PuzzleData
  { input    :: !Element
  , expected :: ![Text]
  , bannedCharacters :: ![Text]
  }

data AppState = Empty | Puzzle PuzzleData

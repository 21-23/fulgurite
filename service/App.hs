module App
  ( App(..)
  ) where

import Config (Config)
import AppState (AppState)
import Control.Concurrent.MVar (MVar)

data App = App
  { config   :: Config
  , stateVar :: MVar AppState
  }

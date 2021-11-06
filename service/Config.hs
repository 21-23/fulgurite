module Config
  ( Config(..)
  , defaultConfig
  ) where

import GHC.Generics (Generic)
import System.Envy (FromEnv)

data Config = Config
  { messengerHost :: String
  , messengerPort :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass FromEnv

defaultConfig :: Config
defaultConfig = Config
  { messengerHost = "127.0.0.1"
  , messengerPort = 3000
  }

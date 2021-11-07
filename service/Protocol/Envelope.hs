module Protocol.Envelope
  ( Envelope(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson      (FromJSON, ToJSON)
import Protocol.ServiceIdentity (ServiceSelector)

data Envelope a = Envelope
  { to      :: ServiceSelector
  , message :: a
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

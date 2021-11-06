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

-- instance (ToJSON msg) => ToJSON (Envelope msg) where
--   toJSON (Envelope to message) = object
--     [ "to"      .= to
--     , "message" .= message
--     ]

-- instance (FromJSON msg) => FromJSON (Envelope msg) where
--   parseJSON (Object envelope) =
--     Envelope
--       <$> envelope .: "to"
--       <*> envelope .: "message"
--   parseJSON _ = fail "Bad message envelope format"

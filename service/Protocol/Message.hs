module Protocol.Message
  ( IncomingMessage(..)
  , OutgoingMessage(..)
  ) where

import Data.Aeson    (FromJSON(parseJSON),
                      Value(Object, String),
                      (.:),
                      ToJSON(toJSON),
                      object,
                      (.=),
                      decode, encode
                      )
import Control.Monad (mzero)
import Data.Text     (Text, unpack)
import Data.UUID (UUID)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)

import Protocol.ServiceIdentity      (ServiceType, ServiceIdentity)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)

data IncomingMessage
  = ServiceCheckIn ServiceIdentity
  | SetSandbox Text [Text] [Text] -- input, expected, banned characters
  | ResetSandbox
  | EvaluateSolution UUID Text

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "checkedIn"         -> ServiceCheckIn <$> message .: "identity"
      String "sandbox.set"       -> do
        options <- message .: "puzzleOptions"
        (String expected) <- message .: "expected"
        case decode (fromStrict $ encodeUtf8 expected) of
          Nothing -> mzero
          Just decoded ->
            SetSandbox <$> message .: "input"
                       <*> pure decoded
                       <*> options .: "bannedCharacters"
      String "sandbox.reset"     -> pure ResetSandbox
      String "solution.evaluate" ->
        EvaluateSolution <$> message .: "taskId"
                         <*> message .: "solution"

      String badName             -> fail $ "Unrecognized message: " <> unpack badName
      _                          -> fail "Message name is not a string"
  parseJSON _ = mzero

{-
  {
    "to":{
      "id":"b9dc4aec-248a-41bf-8342-91ef6ea73fbf",
      "type":"sandbox-service:cssqd"
    },
    "message":{
      "sandboxSettings":{
        "inputCopies":null,"
        refillWorkers":null,
        "reloadWorkers":null,
        "timeout":150
      },
      "input":"<span id=\"me\" data-qdid=\"0\"></span><div id=\"not-me\" data-qdid=\"1\"></div><div id=\"me\" data-qdid=\"2\"></div><span id=\"not-me-either\" data-qdid=\"3\"></div>",
      "puzzleOptions":{
        "timeLimit":150,
        "bannedCharacters":[":","~","+"]
      },
      "name":"sandbox.set",
      "hidden":[],
      "expected":"[\"2\"]"
      }
    }
-}

{-

{
  "to":{
    "id":"b9dc4aec-248a-41bf-8342-91ef6ea73fbf",
    "type":"sandbox-service:cssqd"
  },
  "message":{
    "taskId":"fc0798d0-547a-4225-8f58-659b1c7dbe55",
    "name":"solution.evaluate",
    "solution":"div"
  }
}


-}

{-
  {
    "to":"state-service",
    "message":{
      "name":"solution.evaluated",
      "taskId":"38000689-fa58-48b5-aad4-15d2c63fbaa9",
      "result":"[\"2\"]",
      "correct":"correct"
    }
  }
-}


{-

  {
    "to":"state-service",
    "message":{
      "name":"solution.evaluated",
      "taskId":"ec47d328-fcc5-4b76-9ee5-863aa861f369",
      "error":"Banned characters are not allowed"
    }
  }

-}

{-

  {
    "to":"state-service",
    "message":{
      "name":"solution.evaluated",
      "taskId":"af5348c4-b6c0-4220-8e38-908f140d70a7",
      "result":"[\"0\",\"2\"]",
      "correct":"incorrect"
    }
  }

-}

data OutgoingMessage
  = CheckIn ServiceType
  | CorrectSolution UUID [Text]
  | IncorrectSolution UUID [Text]
  | SolutionError UUID Text

instance ToJSON OutgoingMessage where
  toJSON (CheckIn serviceType) = object
    [ "name"     .= String "checkin"
    , "identity" .= serviceType
    ]

  toJSON (CorrectSolution taskId result) = object
    [ "name"   .= String "solution.evaluated"
    , "taskId" .= taskId
    , "result" .= toStrict (decodeUtf8 $ encode result)
    , "correct" .= String "correct"
    ]

  toJSON (IncorrectSolution taskId result) = object
    [ "name"   .= String "solution.evaluated"
    , "taskId" .= taskId
    , "result" .= toStrict (decodeUtf8 $ encode result)
    , "correct" .= String "incorrect"
    ]

  toJSON (SolutionError taskId errorMessage) = object
    [ "name"   .= String "solution.evaluated"
    , "taskId" .= taskId
    , "error" .= errorMessage
    ]

module Main (main) where

import           Prelude hiding (unwords)
import qualified Network.WebSockets as WebSocket
import qualified Data.Aeson as Aeson
import Control.Monad.Reader (MonadReader(ask), ReaderT(..), runReaderT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Exception (catch)
import Control.Monad (forever)
import Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_)
import System.Envy (decodeWithDefaults)
import System.IO (stderr, stdout, hSetBuffering, BufferMode(LineBuffering))

import App (App(..))
import AppState (AppState(..), PuzzleData (..))
import Config (Config(..), defaultConfig)
import Protocol.Envelope (Envelope(..))
import Protocol.ServiceIdentity (ServiceSelector(..), ServiceType(..))
import Protocol.Message (OutgoingMessage(..), IncomingMessage(..))
import Protocol.Game (Game(..))

import Match (parseElements, parseSelector, matchElement)
import Data.Maybe (catMaybes)
import Element (getAttrValue, Element (..), ElementData(..))
import Data.Text (unwords, isInfixOf, pack)
import Data.List ((\\))

setEq :: Eq a => [a] -> [a] -> Bool
setEq xs ys = null (xs \\ ys) && null (ys \\ xs)

clientApp :: (MonadReader App m, MonadIO m) => WebSocket.Connection -> m ()
clientApp connection = do
  App { stateVar } <- ask
  liftIO $ catch
    (sendMessage Messenger $ CheckIn (SandboxService CSSQuickDraw))
    (\exception -> putStrLn $ "WebSocket error: " <> show (exception :: WebSocket.ConnectionException))
  liftIO $ forever $ do
    string <- WebSocket.receiveData connection
    case Aeson.eitherDecode string :: Either String (Envelope IncomingMessage) of
      Left err -> putStrLn err
      Right Envelope {message} ->
        case message of
          ServiceCheckIn identity -> putStrLn $ "Received identity: " <> show identity
          SetSandbox input expected bannedCharacters ->
            case parseElements input of
              Left err -> putStrLn err
              Right elements ->
                let fragmentElement = Element { content = EData "fragment" [], children = elements }
                 in modifyMVar_ stateVar $ const $ pure $ Puzzle $ PuzzleData fragmentElement expected bannedCharacters
          ResetSandbox ->
            modifyMVar_ stateVar $ const $ pure Empty
          EvaluateSolution taskId solution -> do
            appState <- readMVar stateVar
            case appState of
              Empty -> mempty
              Puzzle (PuzzleData input expected bannedCharacters) ->
                if any (`isInfixOf` solution) bannedCharacters
                  then
                    sendMessage (AnyOfType StateService) $ SolutionError taskId "Banned characters encountered in the solution"
                  else
                    case parseSelector solution of
                      Left err -> sendMessage (AnyOfType StateService) $ SolutionError taskId $ "Invalid selector " <> solution <> "\n\t" <> pack err
                      Right selector ->
                        let matched = matchElement input selector
                            actual = unwords <$> catMaybes (getAttrValue "data-qdid" <$> matched)
                            response | actual `setEq` expected = CorrectSolution taskId actual
                                     | otherwise               = IncorrectSolution taskId actual
                        in sendMessage (AnyOfType StateService) response

  where
    sendMessage selector = WebSocket.sendTextData connection . Aeson.encode . Envelope selector

connectToMessenger :: (MonadReader App m, MonadIO m) => m ()
connectToMessenger = do
  app@App { config = Config { messengerHost, messengerPort } } <- ask
  liftIO $ putStrLn $ "Connecting to " <> messengerHost <> ":" <> show messengerPort
  liftIO $ WebSocket.runClient messengerHost messengerPort "/" (\connection -> runReaderT (clientApp connection) app)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    config <- decodeWithDefaults defaultConfig
    stateVar <- newMVar Empty
    let app = App { config, stateVar }
    runReaderT connectToMessenger app

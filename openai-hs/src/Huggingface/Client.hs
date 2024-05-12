{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp -pgmP "cpphs --cpp" #-}

module Huggingface.Client
  ( -- * Basics
    ApiKey,
    HuggingfaceClient,
    makeHuggingfaceClient,
    makeHuggingfaceClient',
    ClientError (..),

    -- * Helper types
    TimeStamp (..),

    -- * Models
    Model (..),
    ModelId (..),

    -- * Chat
    ChatCompletionRequest (..),
    ChatInputs (..),
    ChatResponse (..),
    completeChat,
    defaultChatCompletionRequest,
  )
where

import Common.Client.Internal.Helpers
import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Huggingface.Api
import Huggingface.Resources
import Network.HTTP.Client (Manager)
import Servant.Auth.Client
import Servant.Client

-- | Your Huggingface API key. Can be obtained from the Huggingface dashboard. Format: @sk-<redacted>@
type ApiKey = T.Text

-- | Holds a 'Manager' and your API key.
data HuggingfaceClient = HuggingfaceClient
  { scBaseUrl :: BaseUrl,
    scToken :: Token,
    scManager :: Manager,
    scMaxRetries :: Int
  }

-- | Construct a 'HuggingfaceClient'. Note that the passed 'Manager' must support https (e.g. via @http-client-tls@)
makeHuggingfaceClient' ::
  BaseUrl ->
  ApiKey ->
  Manager ->
  -- | Number of automatic retries the library should attempt.
  Int ->
  HuggingfaceClient
makeHuggingfaceClient' u k = HuggingfaceClient u (Token (T.encodeUtf8 k))

-- | method using default remote base url
makeHuggingfaceClient ::
  ApiKey ->
  Manager ->
  Int ->
  HuggingfaceClient
makeHuggingfaceClient = makeHuggingfaceClient' openaiBaseUrl

api :: Proxy HuggingfaceApi
api = Proxy

openaiBaseUrl :: BaseUrl
openaiBaseUrl = BaseUrl Https "api-inference.huggingface.co" 443 ""

-- #define EP1(N, ARG, R)

completeChat' ::
  Token ->
  T.Text ->
  T.Text ->
  ChatCompletionRequest ->
  ClientM [ChatResponse]
completeChat' = client api

completeChat ::
  (MonadIO m) =>
  HuggingfaceClient ->
  ChatCompletionRequest ->
  m (Either ClientError [ChatResponse])
completeChat sc a =
  liftIO . runRequest (scMaxRetries sc) 0 $
    runClientM
      (completeChat' (scToken sc) (chcrNamespace a) (unModelId $ chcrModel a) a)
      (mkClientEnv (scManager sc) (scBaseUrl sc))

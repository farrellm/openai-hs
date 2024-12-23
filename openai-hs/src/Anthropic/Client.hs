{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp -pgmP "cpphs --cpp" #-}

module Anthropic.Client
  ( -- * Basics
    ApiKey,
    AnthropicClient,
    makeAnthropicClient,
    makeAnthropicClient',
    ClientError (..),

    -- * Helper types
    TimeStamp (..),
    Usage (..),

    -- * Models
    ModelId (..),

    -- * Chat
    ChatRole (..),
    ChatMessage (..),
    ChatMessageContent (..),
    ChatToolResult (..),
    ChatCompletionRequest (..),
    ChatStopReason (..),
    ChatContent (..),
    ChatContentText (..),
    ChatContentTool (..),
    ChatResponse (..),
    ChatTool (..),
    -- ChatToolType (..),
    defaultChatCompletionRequest,
    completeChat,
  )
where

import Anthropic.Api
import Anthropic.Resources
import Common.Client.Internal.Helpers
import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Servant.Client

-- | Your Anthropic API key. Can be obtained from the Anthropic dashboard. Format: @sk-<redacted>@
type ApiKey = T.Text

-- | Holds a 'Manager' and your API key.
data AnthropicClient = AnthropicClient
  { scBaseUrl :: BaseUrl,
    scToken :: Maybe ApiKey,
    scVersion :: Maybe T.Text,
    scManager :: Manager,
    scMaxRetries :: Int
  }

-- | Construct a 'AnthropicClient'. Note that the passed 'Manager' must support https (e.g. via @http-client-tls@)
makeAnthropicClient' ::
  BaseUrl ->
  ApiKey ->
  T.Text ->
  Manager ->
  -- | Number of automatic retries the library should attempt.
  Int ->
  AnthropicClient
makeAnthropicClient' u k v = AnthropicClient u (Just k) (Just v)

-- | method using default remote base url
makeAnthropicClient ::
  ApiKey ->
  T.Text ->
  Manager ->
  Int ->
  AnthropicClient
makeAnthropicClient = makeAnthropicClient' openaiBaseUrl

api :: Proxy AnthropicApi
api = Proxy

openaiBaseUrl :: BaseUrl
openaiBaseUrl = BaseUrl Https "api.anthropic.com" 443 ""

#define EP0(N, R) \
    N##' :: Maybe T.Text -> Maybe T.Text -> ClientM R;\
    N :: MonadIO m => AnthropicClient -> m (Either ClientError R);\
    N sc = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) (scVersion sc)) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP1(N, ARG, R) \
    N##' :: Maybe T.Text -> Maybe T.Text -> ARG -> ClientM R;\
    N :: MonadIO m => AnthropicClient -> ARG -> m (Either ClientError R);\
    N sc a = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) (scVersion sc) a) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP2(N, ARG, ARG2, R) \
    N##' :: Maybe T.Text -> Maybe T.Text -> ARG -> ARG2 -> ClientM R;\
    N :: MonadIO m => AnthropicClient -> ARG -> ARG2 -> m (Either ClientError R);\
    N sc a b = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) (scVersion sc) a b) (mkClientEnv (scManager sc) (scBaseUrl sc))

EP1 (completeChat, ChatCompletionRequest, ChatResponse)

((completeChat')) =
  client api

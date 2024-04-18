{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp -pgmP "cpphs --cpp" #-}

module Cohere.Client
  ( -- * Basics
    ApiKey,
    CohereClient,
    makeCohereClient,
    makeCohereClient',
    ClientError (..),

    -- * Helper types
    TimeStamp (..),
    Usage (..),

    -- * Models
    Model (..),
    Models (..),
    ModelId (..),
    listModels,
    getModel,

    -- * Chat
    ChatRole (..),
    ChatFunction (..),
    ChatFunctionCall (..),
    ChatFunctionCallStrategy (..),
    ChatMessage (..),
    ChatCompletionRequest (..),
    ChatFinishReason (..),
    ChatResponse (..),
    ChatTool (..),
    ChatToolType (..),
    ChatToolCall (..),
    defaultChatCompletionRequest,
    completeChat,
  )
where

import Cohere.Api
import Cohere.Resources
import Common.Client.Internal.Helpers
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Auth.Client
import Servant.Client
import qualified Servant.Multipart.Client as MP

-- | Your Cohere API key. Can be obtained from the Cohere dashboard. Format: @sk-<redacted>@
type ApiKey = T.Text

-- | Holds a 'Manager' and your API key.
data CohereClient = CohereClient
  { scBaseUrl :: BaseUrl,
    scToken :: Token,
    scManager :: Manager,
    scMaxRetries :: Int
  }

-- | Construct a 'CohereClient'. Note that the passed 'Manager' must support https (e.g. via @http-client-tls@)
makeCohereClient' ::
  BaseUrl ->
  ApiKey ->
  Manager ->
  -- | Number of automatic retries the library should attempt.
  Int ->
  CohereClient
makeCohereClient' u k = CohereClient u (Token (T.encodeUtf8 k))

-- | method using default remote base url
makeCohereClient ::
  ApiKey ->
  Manager ->
  Int ->
  CohereClient
makeCohereClient = makeCohereClient' openaiBaseUrl

api :: Proxy CohereApi
api = Proxy

openaiBaseUrl :: BaseUrl
openaiBaseUrl = BaseUrl Https "api.cohere.com" 443 ""

#define EP0(N, R) \
    N##' :: Token -> ClientM R;\
    N :: MonadIO m => CohereClient -> m (Either ClientError R);\
    N sc = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc)) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP1(N, ARG, R) \
    N##' :: Token -> ARG -> ClientM R;\
    N :: MonadIO m => CohereClient -> ARG -> m (Either ClientError R);\
    N sc a = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) a) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP2(N, ARG, ARG2, R) \
    N##' :: Token -> ARG -> ARG2 -> ClientM R;\
    N :: MonadIO m => CohereClient -> ARG -> ARG2 -> m (Either ClientError R);\
    N sc a b = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) a b) (mkClientEnv (scManager sc) (scBaseUrl sc))

EP0 (listModels, Models)
EP1 (getModel, ModelId, Model)

EP1 (completeChat, ChatCompletionRequest, ChatResponse)

( ( listModels'
      :<|> getModel'
    )
    :<|> (completeChat')
  ) =
    client api

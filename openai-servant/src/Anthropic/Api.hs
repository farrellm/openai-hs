-- | The API
module Anthropic.Api where

import Anthropic.Resources
import Data.Text (Text)
import Servant.API
import Servant.Auth
import Servant.Auth.Client
import Servant.Multipart.API

-- type AnthropicAuth = Auth '[Bearer] ()
type AnthropicAuth = Header "x-api-key" Text

type AnthropicVersion = Header "anthropic-version" Text

type AnthropicApi =
  "v1" :> AnthropicApiInternal

type AnthropicApiInternal =
  "messages" :> ChatApi

type ChatApi =
  AnthropicAuth :> AnthropicVersion :> ReqBody '[JSON] ChatCompletionRequest :> Post '[JSON] ChatResponse

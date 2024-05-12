-- | The API
module Huggingface.Api where

import qualified Data.Text as T
import Huggingface.Resources
import Servant.API
import Servant.Auth
import Servant.Auth.Client

type HuggingfaceAuth = Auth '[Bearer] ()

type HuggingfaceApi =
  "models" :> ChatApi

type ChatApi =
  HuggingfaceAuth
    :> Capture "namespace" T.Text
    :> Capture "model" T.Text
    :> ReqBody '[JSON] ChatCompletionRequest
    :> Post '[JSON] [ChatResponse]

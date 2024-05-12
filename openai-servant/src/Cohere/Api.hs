-- | The API
module Cohere.Api where

import Cohere.Resources
import Servant.API
import Servant.Auth
import Servant.Auth.Client

type CohereAuth = Auth '[Bearer] ()

type CohereApi =
  "v1" :> CohereApiInternal

type CohereApiInternal =
  "models" :> ModelsApi
    :<|> "chat" :> ChatApi

type ModelsApi =
  CohereAuth :> Get '[JSON] Models
    :<|> CohereAuth :> Capture "model_id" ModelId :> Get '[JSON] Model

type ChatApi =
  CohereAuth :> ReqBody '[JSON] ChatCompletionRequest :> Post '[JSON] ChatResponse

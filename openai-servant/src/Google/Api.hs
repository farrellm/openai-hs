-- | The API
module Google.Api where

import qualified Data.Text as T
import Google.Resources
import Servant.API

type GoogleApi =
  "v1beta" :> GoogleApiInternal

type GoogleApiInternal =
  "models" :> ModelsApi

type ModelsApi =
  Capture "model" T.Text
    :> QueryParam "key" T.Text
    :> ReqBody '[JSON] GenerateContentRequest
    :> Post '[JSON] GenerateResponse

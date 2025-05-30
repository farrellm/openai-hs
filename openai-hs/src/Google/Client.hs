{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp -pgmP "cpphs --cpp" #-}

module Google.Client
  ( GoogleClient (..),
    makeGoogleClient,
    generateContent,
    ClientError (..),
    module Google.Resources,
  )
where

import Common.Client.Internal.Helpers
import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy
import qualified Data.Text as T
import Google.Api
import Google.Resources
import Network.HTTP.Client (Manager)
import Servant.Client

-- | Your Google API key. Can be obtained from the Google dashboard. Format: @sk-<redacted>@
type ApiKey = T.Text

-- | Holds a 'Manager' and your API key.
data GoogleClient = GoogleClient
  { scBaseUrl :: BaseUrl,
    scToken :: T.Text,
    scManager :: Manager,
    scMaxRetries :: Int
  }

-- | Construct a 'GoogleClient'. Note that the passed 'Manager' must support https (e.g. via @http-client-tls@)
makeGoogleClient' ::
  BaseUrl ->
  ApiKey ->
  Manager ->
  -- | Number of automatic retries the library should attempt.
  Int ->
  GoogleClient
makeGoogleClient' u k = GoogleClient u k

-- | method using default remote base url
makeGoogleClient ::
  ApiKey ->
  Manager ->
  Int ->
  GoogleClient
makeGoogleClient = makeGoogleClient' openaiBaseUrl

api :: Proxy GoogleApi
api = Proxy

openaiBaseUrl :: BaseUrl
openaiBaseUrl = BaseUrl Https "generativelanguage.googleapis.com" 443 ""

generateContent ::
  (MonadIO m) =>
  GoogleClient ->
  GenerateContentRequest ->
  m (Either ClientError GenerateResponse)
generateContent sc a =
  liftIO . runRequest (scMaxRetries sc) 0 $
    runClientM
      ( generateContent' (gcr_model a <> ":generateContent") (Just $ scToken sc) a -- {gcrModel = Nothing}
      )
      (mkClientEnv (scManager sc) (scBaseUrl sc))

generateContent' ::
  T.Text ->
  Maybe T.Text ->
  GenerateContentRequest ->
  ClientM GenerateResponse
(generateContent') =
  client api

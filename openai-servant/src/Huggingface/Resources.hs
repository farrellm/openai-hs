{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Huggingface.Resources
  ( -- * Core Types
    TimeStamp (..),
    Model (..),
    ModelId (..),

    -- * Chat
    ChatCompletionRequest (..),
    ChatInputs (..),
    ChatResponse (..),
    defaultChatCompletionRequest,
  )
where

import Common.Internal.Aeson
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Servant.API

-- | A 'UTCTime' wrapper that has unix timestamp JSON representation
newtype TimeStamp = TimeStamp {unTimeStamp :: UTCTime}
  deriving (Show, Eq)

instance A.ToJSON TimeStamp where
  toJSON = A.Number . fromRational . toRational . utcTimeToPOSIXSeconds . unTimeStamp

instance A.FromJSON TimeStamp where
  parseJSON =
    A.withScientific "unix timestamp" $ \sci ->
      pure $ TimeStamp $ posixSecondsToUTCTime (fromRational $ toRational sci)

instance ToHttpApiData TimeStamp where
  toUrlPiece x =
    let unix :: Int
        unix = round . utcTimeToPOSIXSeconds . unTimeStamp $ x
     in T.pack (show unix)

newtype Model = Model
  { mName :: T.Text
  }
  deriving (Show, Eq)

newtype ModelId = ModelId {unModelId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

------------------------
------ Chat API
------------------------

data ChatInputs = ChatInputs
  { chiText :: T.Text,
    chiGeneratedResponses :: [T.Text],
    chiPastUserInputs :: [T.Text]
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''ChatInputs)

data ChatCompletionRequest = ChatCompletionRequest
  { chcrModel :: ModelId,
    chcrNamespace :: T.Text,
    chcrInputs :: T.Text
    -- chcrPreamble :: Maybe T.Text,
    -- chcrChatHistory :: [ChatMessage],
    -- chcrTemperature :: Maybe Double,
    -- chcrMaxTokens :: Maybe Int,
    -- chcrMaxInputTokens :: Maybe Int,
    -- chcrK :: Maybe Int,
    -- chcrP :: Maybe Double,
    -- chcrStopSequences :: Maybe (V.Vector T.Text),
    -- chcrFrequencyPenalty :: Maybe Double,
    -- chcrPresencePenalty :: Maybe Double
  }
  deriving (Show, Eq)

instance A.ToJSON ChatCompletionRequest where
  toJSON (ChatCompletionRequest {chcrInputs = inputs}) =
    A.object
      [ "inputs" A..= A.toJSON inputs
      ]

defaultChatCompletionRequest :: T.Text -> ModelId -> T.Text -> ChatCompletionRequest
defaultChatCompletionRequest namespace model message =
  ChatCompletionRequest
    { chcrModel = model,
      chcrNamespace = namespace,
      chcrInputs = message
      -- chcrMessage = message,
      -- chcrPreamble = Nothing,
      -- chcrChatHistory = [],
      -- chcrTemperature = Nothing,
      -- chcrMaxTokens = Nothing,
      -- chcrMaxInputTokens = Nothing,
      -- chcrK = Nothing,
      -- chcrP = Nothing,
      -- chcrStopSequences = Nothing,
      -- chcrPresencePenalty = Nothing,
      -- chcrFrequencyPenalty = Nothing
    }

newtype ChatResponse = ChatResponse
  { chrGeneratedText :: T.Text
  -- chrConversation :: T.Text
  }
  deriving (Show)

$(deriveJSON (jsonOpts 3) ''ChatResponse)

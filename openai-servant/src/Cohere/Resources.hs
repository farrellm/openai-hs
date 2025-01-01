{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Cohere.Resources
  ( -- * Core Types
    TimeStamp (..),
    Usage (..),

    -- * Models
    Models (..),
    Model (..),
    ModelId (..),

    -- * Chat
    ChatRole (..),
    ChatMessage (..),
    ChatCompletionRequest (..),
    ChatResponse (..),
    ChatTool (..),
    ChatToolResult (..),
    ChatToolCall (..),
    ChatParameterDefinition (..),
    defaultChatCompletionRequest,
  )
where

import Common.Internal.Aeson
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
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

data Usage = Usage
  { usPromptTokens :: Int,
    usCompletionTokens :: Int,
    usTotalTokens :: Int
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''Usage)

------------------------
------ Model API
------------------------

newtype Model = Model
  { mName :: T.Text
  }
  deriving (Show, Eq)

newtype ModelId = ModelId {unModelId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Models = CohereList
  { msModels :: V.Vector Model,
    msNextPageToken :: Maybe T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 1) ''Model)
$(deriveJSON (jsonOpts 2) ''Models)

------------------------
------ Chat API
------------------------

data ChatRole
  = CR_SYSTEM
  | CR_USER
  | CR_CHATBOT
  | CR_TOOL
  deriving (Show, Eq)

$(deriveJSON (jsonOpts'' 3) ''ChatRole)

data ChatToolCall = ChatToolCall
  { chtcName :: T.Text,
    chtcParameters :: A.Object
  }
  deriving (Eq, Show)

$(deriveJSON (jsonOpts 4) ''ChatToolCall)

data ChatMessage = ChatMessage
  { chmRole :: ChatRole,
    chmMessage :: Maybe T.Text,
    chmToolResults :: Maybe [A.Object]
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''ChatMessage)

data ChatParameterDefinition = ChatParameterDefinition
  { chpdDescription :: T.Text,
    chpdType :: T.Text,
    chpdRequired :: Bool
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 4) ''ChatParameterDefinition)

data ChatTool = ChatTool
  { chtName :: T.Text,
    chtDescription :: T.Text,
    chtParameterDefinitions :: M.Map T.Text ChatParameterDefinition
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''ChatTool)

data ChatToolResult = ChatToolResult
  { chtrCall :: ChatToolCall,
    chtrOutputs :: V.Vector A.Object
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 4) ''ChatToolResult)

data ChatCompletionRequest = ChatCompletionRequest
  { chcrModel :: ModelId,
    chcrMessage :: T.Text,
    chcrPreamble :: Maybe T.Text,
    chcrChatHistory :: [ChatMessage],
    chcrTemperature :: Maybe Double,
    chcrMaxTokens :: Maybe Int,
    chcrMaxInputTokens :: Maybe Int,
    chcrK :: Maybe Int,
    chcrP :: Maybe Double,
    chcrStopSequences :: Maybe (V.Vector T.Text),
    chcrFrequencyPenalty :: Maybe Double,
    chcrPresencePenalty :: Maybe Double,
    chcrTools :: Maybe [ChatTool],
    chcrToolResults :: Maybe [ChatToolResult]
  }
  deriving (Show, Eq)

defaultChatCompletionRequest :: ModelId -> T.Text -> ChatCompletionRequest
defaultChatCompletionRequest model message =
  ChatCompletionRequest
    { chcrModel = model,
      chcrMessage = message,
      chcrPreamble = Nothing,
      chcrChatHistory = [],
      chcrTemperature = Nothing,
      chcrMaxTokens = Nothing,
      chcrMaxInputTokens = Nothing,
      chcrK = Nothing,
      chcrP = Nothing,
      chcrStopSequences = Nothing,
      chcrPresencePenalty = Nothing,
      chcrFrequencyPenalty = Nothing,
      chcrTools = Nothing,
      chcrToolResults = Nothing
    }

data ChatResponse = ChatResponse
  { chrResponseId :: T.Text,
    chrText :: T.Text,
    chrGenerationId :: T.Text,
    chrToolCalls :: Maybe [ChatToolCall],
    chrChatHistory :: [ChatMessage]
  }
  deriving (Show)

$(deriveJSON (jsonOpts 4) ''ChatCompletionRequest)
$(deriveJSON (jsonOpts 3) ''ChatResponse)

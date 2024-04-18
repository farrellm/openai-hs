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
    ChatFunction (..),
    ChatFunctionCall (..),
    ChatFunctionCallStrategy (..),
    ChatFinishReason (..),
    ChatMessage (..),
    ChatCompletionRequest (..),
    ChatResponse (..),
    ChatTool (..),
    ChatToolType (..),
    ChatToolCall (..),
    defaultChatCompletionRequest,
  )
where

import Common.Internal.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import Network.Mime (defaultMimeLookup)
import Servant.API
import Servant.Multipart.API

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

data Model = Model
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
  deriving (Show, Eq)

$(deriveJSON (jsonOpts' 3) ''ChatRole)

data ChatFunctionCall = ChatFunctionCall
  { chfcName :: T.Text,
    chfcArguments :: A.Value
  }
  deriving (Eq, Show)

instance A.FromJSON ChatFunctionCall where
  parseJSON = A.withObject "ChatFunctionCall" $ \obj -> do
    name <- obj A..: "name"
    arguments <- obj A..: "arguments" >>= A.withEmbeddedJSON "Arguments" pure

    pure $ ChatFunctionCall {chfcName = name, chfcArguments = arguments}

instance A.ToJSON ChatFunctionCall where
  toJSON (ChatFunctionCall {chfcName = name, chfcArguments = arguments}) =
    A.object
      [ "name" A..= name,
        "arguments" A..= T.decodeUtf8 (BSL.toStrict (A.encode arguments))
      ]

data ChatToolCall = ChatToolCall
  { chtcId :: T.Text,
    chtcType :: T.Text,
    chtcFunction :: ChatFunctionCall
  }
  deriving (Eq, Show)

$(deriveJSON (jsonOpts 4) ''ChatToolCall)

data ChatMessage = ChatMessage
  { chmRole :: ChatRole,
    chmMessage :: T.Text
  }
  deriving (Show, Eq)

instance A.FromJSON ChatMessage where
  parseJSON = A.withObject "ChatMessage" $ \obj ->
    ChatMessage
      <$> obj A..: "role"
      <*> obj A..: "message"

instance A.ToJSON ChatMessage where
  toJSON (ChatMessage {chmMessage = message, chmRole = role}) =
    A.object $
      [ "role" A..= role,
        "message" A..= message
      ]

data ChatFunction = ChatFunction
  { chfName :: T.Text,
    chfDescription :: T.Text,
    chfParameters :: Maybe A.Value
  }
  deriving (Show, Eq)

data ChatFunctionCallStrategy
  = CFCS_auto
  | CFCS_none
  | CFCS_name T.Text
  deriving (Show, Eq)

instance ToJSON ChatFunctionCallStrategy where
  toJSON = \case
    CFCS_auto -> A.String "auto"
    CFCS_none -> A.String "none"
    CFCS_name functionName -> A.object ["name" A..= A.toJSON functionName]

instance FromJSON ChatFunctionCallStrategy where
  parseJSON (A.String "auto") = pure CFCS_auto
  parseJSON (A.String "none") = pure CFCS_none
  parseJSON xs = flip (A.withObject "ChatFunctionCallStrategy") xs $ \o -> do
    functionName <- o A..: "name"
    pure $ CFCS_name functionName

data ChatToolType
  = CTT_function
  deriving (Show, Eq)

data ChatTool = ChatTool
  { chtType :: ChatToolType,
    chtFunction :: ChatFunction
  }
  deriving (Show, Eq)

data ChatToolChoice
  = CTC_auto
  | CTC_none
  | CTC_name T.Text
  deriving (Show, Eq)

instance ToJSON ChatToolChoice where
  toJSON = \case
    CTC_auto -> A.String "auto"
    CTC_none -> A.String "none"
    CTC_name functionName ->
      A.object
        [ "type" A..= A.String "function",
          "function" A..= A.object ["name" A..= A.toJSON functionName]
        ]

instance FromJSON ChatToolChoice where
  parseJSON (A.String "auto") = pure CTC_auto
  parseJSON (A.String "none") = pure CTC_none
  parseJSON xs = flip (A.withObject "ChatToolChoice") xs $ \o -> do
    functionName <- o A..: "name"
    pure $ CTC_name functionName

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
    chcrPresencePenalty :: Maybe Double
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
      chcrFrequencyPenalty = Nothing
    }

data ChatFinishReason
  = CFR_stop
  | CFR_length
  | CFR_contentFilter
  | CFR_toolCalls
  deriving (Show, Eq)

data ChatResponse = ChatResponse
  { chrResponseId :: T.Text,
    chrText :: T.Text,
    chrGenerationId :: T.Text
  }
  deriving (Show)

$(deriveJSON (jsonOpts 3) ''ChatFunction)
$(deriveJSON (jsonOpts' 4) ''ChatToolType)
$(deriveJSON (jsonOpts 3) ''ChatTool)
$(deriveJSON (jsonOpts 4) ''ChatCompletionRequest)
$(deriveJSON (jsonOpts' 4) ''ChatFinishReason)
$(deriveJSON (jsonOpts 3) ''ChatResponse)

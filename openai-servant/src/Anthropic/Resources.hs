{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Anthropic.Resources
  ( -- * Core Types
    TimeStamp (..),
    Usage (..),
    ModelId (..),

    -- * Chat
    ChatRole (..),
    ChatFunction (..),
    ChatFunctionCall (..),
    ChatFunctionCallStrategy (..),
    ChatStopReason (..),
    ChatContent (..),
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

$(deriveJSON (jsonOpts 1) ''Model)

------------------------
------ Chat API
------------------------

data ChatRole
  = CR_system
  | CR_user
  | CR_assistant
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
    chmContent :: T.Text
  }
  deriving (Show, Eq)

instance A.FromJSON ChatMessage where
  parseJSON = A.withObject "ChatMessage" $ \obj ->
    ChatMessage
      <$> obj A..: "role"
      <*> obj A..: "content"

instance A.ToJSON ChatMessage where
  toJSON (ChatMessage {chmContent = content, chmRole = role}) =
    A.object $
      [ "role" A..= role,
        "content" A..= content
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
    chcrMessages :: [ChatMessage],
    chcrMaxTokens :: Int,
    chcrStopSequences :: Maybe (V.Vector T.Text),
    chcrSystem :: Maybe T.Text,
    chcrTemperature :: Maybe Double,
    chcrTopK :: Maybe Int,
    chcrTopP :: Maybe Double
  }
  deriving (Show, Eq)

defaultChatCompletionRequest :: ModelId -> [ChatMessage] -> ChatCompletionRequest
defaultChatCompletionRequest model messages =
  ChatCompletionRequest
    { chcrModel = model,
      chcrMessages = messages,
      chcrMaxTokens = 1000,
      chcrStopSequences = Nothing,
      chcrSystem = Nothing,
      chcrTemperature = Nothing,
      chcrTopK = Nothing,
      chcrTopP = Nothing
    }

data ChatStopReason
  = CSR_end_turn
  | CSR_max_tokens
  | CSR_stop_sequence
  deriving (Show, Eq)

data ChatContent = ChatCobtent
  { chcType :: T.Text,
    chcText :: T.Text
  }
  deriving (Show)

data ChatResponse = ChatResponse
  { chrId :: T.Text,
    chrType :: T.Text,
    chrRole :: ChatRole,
    chrContent :: [ChatContent],
    chrModel :: T.Text,
    chrStopReason :: ChatStopReason
  }
  deriving (Show)

$(deriveJSON (jsonOpts 3) ''ChatFunction)
$(deriveJSON (jsonOpts' 4) ''ChatToolType)
$(deriveJSON (jsonOpts 3) ''ChatTool)
$(deriveJSON (jsonOpts 4) ''ChatCompletionRequest)
$(deriveJSON (jsonOpts' 4) ''ChatStopReason)
$(deriveJSON (jsonOpts 3) ''ChatContent)
$(deriveJSON (jsonOpts 3) ''ChatResponse)

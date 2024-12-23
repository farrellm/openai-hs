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
    ChatStopReason (..),
    ChatContent (..),
    ChatContentText (..),
    ChatContentTool (..),
    ChatMessage (..),
    ChatMessageContent (..),
    ChatToolResult (..),
    ChatCompletionRequest (..),
    ChatResponse (..),
    ChatTool (..),
    -- ChatToolType (..),
    defaultChatCompletionRequest,
  )
where

import Common.Internal.Aeson
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
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

$(deriveJSON (jsonOpts 1) ''Model)

------------------------
------ Chat API
------------------------

data ChatRole
  = CR_user
  | CR_assistant
  deriving (Show, Eq)

$(deriveJSON (jsonOpts' 3) ''ChatRole)

data ChatContentTool = ChatContentTool
  { chctType :: T.Text,
    chctId :: T.Text,
    chctName :: T.Text,
    chctInput :: A.Object
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 4) ''ChatContentTool)

data ChatContentText = ChatContentText
  { chcxType :: T.Text,
    chcxText :: T.Text
  }
  deriving (Show, Eq)

data ChatContent
  = CCText ChatContentText
  | CCTool ChatContentTool
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 4) ''ChatContentText)

instance A.FromJSON ChatContent where
  parseJSON v = CCText <$> A.parseJSON v <|> CCTool <$> A.parseJSON v

instance A.ToJSON ChatContent where
  toJSON (CCText x) = A.toJSON x
  toJSON (CCTool x) = A.toJSON x

data ChatToolResult = ChatToolResult
  { chtrType :: T.Text,
    chtrToolUseId :: T.Text,
    chtrContent :: T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 4) ''ChatToolResult)

data ChatMessageContent
  = CMCText T.Text
  | CMCToolUse [ChatContent]
  | CMCToolResult [ChatToolResult]
  deriving (Show, Eq)

instance A.FromJSON ChatMessageContent where
  parseJSON v =
    CMCText <$> A.parseJSON v
      <|> CMCToolUse <$> A.parseJSON v
      <|> CMCToolResult <$> A.parseJSON v

instance A.ToJSON ChatMessageContent where
  toJSON (CMCText x) = A.toJSON x
  toJSON (CMCToolUse x) = A.toJSON x
  toJSON (CMCToolResult x) = A.toJSON x

data ChatMessage = ChatMessage
  { chmRole :: ChatRole,
    chmContent :: ChatMessageContent
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

-- data JSONSchema = JSONSchema
--   {
--   }
--   deriving (Show, Eq)

data ChatTool = ChatTool
  { chtName :: T.Text,
    chtDescription :: T.Text,
    chtInputSchema :: A.Value
  }
  deriving (Show, Eq)

-- data ChatToolChoice
--   = CTC_auto
--   | CTC_none
--   | CTC_name T.Text
--   deriving (Show, Eq)

-- instance ToJSON ChatToolChoice where
--   toJSON = \case
--     CTC_auto -> A.String "auto"
--     CTC_none -> A.String "none"
--     CTC_name functionName ->
--       A.object
--         [ "type" A..= A.String "function",
--           "function" A..= A.object ["name" A..= A.toJSON functionName]
--         ]

-- instance FromJSON ChatToolChoice where
--   parseJSON (A.String "auto") = pure CTC_auto
--   parseJSON (A.String "none") = pure CTC_none
--   parseJSON xs = flip (A.withObject "ChatToolChoice") xs $ \o -> do
--     functionName <- o A..: "name"
--     pure $ CTC_name functionName

data ChatCompletionRequest = ChatCompletionRequest
  { chcrModel :: ModelId,
    chcrMessages :: [ChatMessage],
    chcrMaxTokens :: Int,
    chcrStopSequences :: Maybe (V.Vector T.Text),
    chcrSystem :: Maybe T.Text,
    chcrTemperature :: Maybe Double,
    chcrTopK :: Maybe Int,
    chcrTopP :: Maybe Double,
    chcrTools :: Maybe [ChatTool]
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
      chcrTopP = Nothing,
      chcrTools = Nothing
    }

data ChatStopReason
  = CSR_end_turn
  | CSR_max_tokens
  | CSR_stop_sequence
  | CSR_tool_use
  deriving (Show, Eq)

data ChatResponse = ChatResponse
  { chrId :: T.Text,
    chrType :: T.Text,
    chrRole :: ChatRole,
    chrContent :: [ChatContent],
    chrModel :: T.Text,
    chrStopReason :: ChatStopReason
  }
  deriving (Show)

-- $(deriveJSON (jsonOpts' 4) ''ChatToolType)

-- $(deriveJSON (jsonOpts 2) ''JSONSchema)

$(deriveJSON (jsonOpts 3) ''ChatTool)
$(deriveJSON (jsonOpts 4) ''ChatCompletionRequest)
$(deriveJSON (jsonOpts' 4) ''ChatStopReason)

-- $(deriveJSON (jsonOpts 3) ''ChatContent)

$(deriveJSON (jsonOpts 3) ''ChatResponse)

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenAI.Resources
  ( -- * Core Types
    TimeStamp (..),
    OpenAIList (..),
    Usage (..),

    -- * Models
    Model (..),
    ModelId (..),

    -- * Completion
    CompletionCreate (..),
    CompletionChoice (..),
    CompletionResponse (..),
    defaultCompletionCreate,

    -- * Chat
    ChatRole (..),
    ChatFunction (..),
    ChatFunctionCall (..),
    ChatFunctionCallStrategy (..),
    ChatFinishReason (..),
    ChatMessage (..),
    ChatCompletionRequest (..),
    ChatChoice (..),
    ChatResponse (..),
    ChatResponseFormat (..),
    ChatTool (..),
    ChatToolType (..),
    ChatToolCall (..),
    ChatContent (..),
    ChatContentPart (..),
    ChatPrediction (..),
    ChatPredictionType (..),
    defaultChatCompletionRequest,

    -- * Images
    ImageResponse (..),
    ImageResponseData (..),
    ImageResponseFormat (..),
    ImageCreate (..),
    ImageEditRequest (..),
    ImageVariationRequest (..),

    -- * Embeddings
    EmbeddingCreate (..),
    EmbeddingResponseData (..),
    EmbeddingUsage (..),
    EmbeddingResponse (..),

    -- * Audio
    AudioResponseData (..),
    AudioTranscriptionRequest (..),
    AudioTranslationRequest (..),

    -- * Fine tuning (out of date)
    FineTuneId (..),
    FineTuneCreate (..),
    defaultFineTuneCreate,
    FineTune (..),
    FineTuneEvent (..),

    -- * File API (out of date)
    FileCreate (..),
    FileId (..),
    File (..),
    FileHunk (..),
    FineTuneHunk (..),
    FileDeleteConfirmation (..),

    -- * Engine (deprecated)
    EngineId (..),
    Engine (..),

    -- * Engine text completion (deprecated)
    TextCompletionId (..),
    TextCompletionChoice (..),
    TextCompletion (..),
    TextCompletionCreate (..),
    defaultEngineTextCompletionCreate,

    -- * Engine Embeddings (deprecated)
    EngineEmbeddingCreate (..),
    EngineEmbedding (..),
  )
where

import Common.Internal.Aeson
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (catMaybes)
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import GHC.Exts (IsList)
import Network.Mime (defaultMimeLookup)
import Servant.API
import Servant.Multipart.API

-- | A 'UTCTime' wrapper that has unix timestamp JSON representation
newtype TimeStamp = TimeStamp {unTimeStamp :: UTCTime}
  deriving stock (Show, Eq)

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

-- | A 'V.Vector' wrapper.
newtype OpenAIList a = OpenAIList
  { olData :: V.Vector a
  }
  deriving stock (Eq, Functor)
  deriving newtype (Applicative, IsList, Monoid, Semigroup, Show)

$(deriveJSON (jsonOpts 2) ''OpenAIList)

data UsageDetails = UsageDetails
  { usdReasoningTokens :: Int,
    usdAcceptedPredictionTokens :: Int,
    usdRejectedPredictionTokens :: Int
  }
  deriving (Show, Eq)

data Usage = Usage
  { usPromptTokens :: Int,
    usCompletionTokens :: Int,
    usTotalTokens :: Int,
    usCompletionTokensDetails :: Maybe UsageDetails
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''UsageDetails)
$(deriveJSON (jsonOpts 2) ''Usage)

instance Semigroup UsageDetails where
  UsageDetails a1 a2 a3 <> UsageDetails b1 b2 b3 =
    UsageDetails (a1 + b1) (a2 + b2) (a3 + b3)

instance Monoid UsageDetails where
  mempty = UsageDetails 0 0 0

instance Semigroup Usage where
  Usage a1 a2 a3 a4 <> Usage b1 b2 b3 b4 =
    Usage (a1 + b1) (a2 + b2) (a3 + b3) (a4 <> b4)

instance Monoid Usage where
  mempty = Usage 0 0 0 mempty

------------------------
------ Model API
------------------------

data Model = Model
  { mId :: ModelId,
    mObject :: T.Text,
    mOwnedBy :: T.Text
  }
  deriving (Show, Eq)

newtype ModelId = ModelId {unModelId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

$(deriveJSON (jsonOpts 1) ''Model)

------------------------
------ Completions API (legacy)
------------------------

data CompletionCreate = CompletionCreate
  { ccrModel :: ModelId,
    ccrPrompt :: Maybe T.Text,
    ccrSuffix :: Maybe T.Text,
    ccrMaxTokens :: Maybe Int,
    ccrTemperature :: Maybe Double,
    ccrTopP :: Maybe Double,
    ccrN :: Maybe Int,
    ccrStream :: Maybe Bool,
    ccrLogprobs :: Maybe Int,
    ccrEcho :: Maybe Bool,
    ccrStop :: Maybe (V.Vector T.Text),
    ccrPresencePenalty :: Maybe Double,
    ccrFrequencyPenalty :: Maybe Double,
    ccrBestOf :: Maybe Int,
    ccrLogitBias :: Maybe (V.Vector Double),
    ccrUser :: Maybe String
  }
  deriving (Show, Eq)

defaultCompletionCreate :: ModelId -> T.Text -> CompletionCreate
defaultCompletionCreate model prompt =
  CompletionCreate
    { ccrModel = model,
      ccrPrompt = Just prompt,
      ccrSuffix = Nothing,
      ccrMaxTokens = Nothing,
      ccrTemperature = Nothing,
      ccrTopP = Nothing,
      ccrN = Nothing,
      ccrStream = Nothing,
      ccrLogprobs = Nothing,
      ccrEcho = Nothing,
      ccrStop = Nothing,
      ccrPresencePenalty = Nothing,
      ccrFrequencyPenalty = Nothing,
      ccrBestOf = Nothing,
      ccrLogitBias = Nothing,
      ccrUser = Nothing
    }

data CompletionChoice = CompletionChoice
  { cchText :: T.Text,
    cchIndex :: Int,
    cchLogprobs :: Maybe (V.Vector Double),
    cchFinishReason :: Maybe T.Text
  }
  deriving (Show, Eq)

data CompletionResponse = CompletionResponse
  { crId :: T.Text,
    crObject :: T.Text,
    crCreated :: Int,
    crModel :: ModelId,
    crChoices :: [CompletionChoice],
    crUsage :: A.Object
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''CompletionCreate)
$(deriveJSON (jsonOpts 3) ''CompletionChoice)
$(deriveJSON (jsonOpts 2) ''CompletionResponse)

------------------------
------ Chat API
------------------------

data ChatRole
  = CR_system
  | CR_developer
  | CR_user
  | CR_assistant
  | CR_tool
  | CR_model
  deriving (Show, Eq)

$(deriveJSON (jsonOpts' 3) ''ChatRole)

data ChatFunctionCall = ChatFunctionCall
  { chfcName :: T.Text,
    chfcArguments :: A.Object
  }
  deriving (Eq, Show)

instance A.FromJSON ChatFunctionCall where
  parseJSON = A.withObject "ChatFunctionCall" $ \obj -> do
    name <- obj A..: "name"
    arguments <- obj A..: "arguments" >>= A.withEmbeddedJSON "Arguments" A.parseJSON

    pure $ ChatFunctionCall {chfcName = name, chfcArguments = arguments}

instance A.ToJSON ChatFunctionCall where
  toJSON (ChatFunctionCall {chfcName = name, chfcArguments = arguments}) =
    A.object
      [ "name" A..= name,
        "arguments" A..= T.decodeUtf8 (BSL.toStrict (A.encode arguments))
      ]

data ChatToolType
  = CTT_function
  deriving (Show, Eq)

$(deriveJSON (jsonOpts' 4) ''ChatToolType)

data ChatToolCall = ChatToolCall
  { chtcId :: T.Text,
    chtcType :: ChatToolType,
    chtcFunction :: ChatFunctionCall
  }
  deriving (Eq, Show)

$(deriveJSON (jsonOpts 4) ''ChatToolCall)

data ChatContentPart = ChatContentPart
  { chcpType :: T.Text,
    chcpText :: T.Text
  }
  deriving (Eq, Show)

$(deriveJSON (jsonOpts 4) ''ChatContentPart)

data ChatContent
  = ContentText T.Text
  | ContentArray [ChatContentPart]
  deriving (Eq, Show)

instance IsString ChatContent where
  fromString = ContentText . T.pack

instance A.FromJSON ChatContent where
  parseJSON = A.withText "ChatContent" $ pure . ContentText

instance A.ToJSON ChatContent where
  toJSON (ContentText t) = A.toJSON t
  toJSON (ContentArray t) = A.toJSON t

data ChatMessage = ChatMessage
  { chmContent :: Maybe ChatContent,
    chmRole :: ChatRole,
    chmName :: Maybe T.Text,
    chmToolCalls :: Maybe [ChatToolCall],
    chmToolCallId :: Maybe T.Text
  }
  deriving (Show, Eq)

instance A.FromJSON ChatMessage where
  parseJSON = A.withObject "ChatMessage" $ \obj ->
    ChatMessage
      <$> obj A..:? "content"
      <*> obj A..: "role"
      <*> obj A..:? "name"
      <*> obj A..:? "tool_calls"
      <*> obj A..:? "tool_call_id"

instance A.ToJSON ChatMessage where
  toJSON (ChatMessage {chmContent = content, chmRole = role, chmName = name, chmToolCalls = toolCalls, chmToolCallId = toolCallId}) =
    A.object $
      [ "content" A..= content,
        "role" A..= role
      ]
        ++ catMaybes
          [ ("tool_calls" A..=) <$> toolCalls,
            ("name" A..=) <$> name,
            ("tool_call_id" A..=) <$> toolCallId
          ]

data ChatJsonSchema = ChatJsonSchema
  { chjsName :: T.Text,
    chjsStrict :: Bool,
    chjsSchema :: A.Value,
    chjsRequired :: [T.Text],
    chjsAdditionalProperties :: Bool
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 4) ''ChatJsonSchema)

data ChatFunction = ChatFunction
  { chfName :: T.Text,
    chfDescription :: T.Text,
    chfParameters :: Maybe A.Value,
    chfStrict :: Maybe Bool
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

data ChatPredictionType = CPT_content
  deriving (Show, Eq)

data ChatPrediction = ChatPrediction
  { chpType :: ChatPredictionType,
    chpContent :: T.Text
  }
  deriving (Show, Eq)

data ChatCompletionRequest = ChatCompletionRequest
  { chcrModel :: ModelId,
    chcrMessages :: [ChatMessage],
    chcrTools :: Maybe [ChatTool],
    chcrToolChoice :: Maybe ChatToolChoice,
    chcrTemperature :: Maybe Double,
    chcrTopP :: Maybe Double,
    chcrN :: Maybe Int,
    chcrPrediction :: Maybe ChatPrediction,
    chcrSeed :: Maybe Int,
    chcrStream :: Maybe Bool,
    chcrStop :: Maybe (V.Vector T.Text),
    chcrMaxCompletionTokens :: Maybe Int,
    chcrPresencePenalty :: Maybe Double,
    chcrResponseFormat :: Maybe ChatResponseFormat,
    chcrFrequencyPenalty :: Maybe Double,
    chcrLogitBias :: Maybe (V.Vector Double),
    chcrUser :: Maybe String
  }
  deriving (Show, Eq)

data ChatResponseFormat
  = RF_text
  | RF_json_object
  | RF_json_schema ChatJsonSchema
  deriving (Show, Eq)

instance ToJSON ChatResponseFormat where
  toJSON = \case
    RF_text -> A.object ["type" A..= A.String "text"]
    RF_json_object -> A.object ["type" A..= A.String "json_object"]
    (RF_json_schema s) ->
      A.object
        [ "type" A..= T.pack "json_schema",
          "json_schema" A..= A.toJSON s
        ]

instance FromJSON ChatResponseFormat where
  parseJSON = A.withObject "ChatResponseFormat" $ \o -> do
    rt <- o A..: "type"
    case rt of
      "text" ->
        pure RF_text
      "json_object" ->
        pure RF_json_object
      xs ->
        fail $ "ChatResponseFormat unexpected type: " <> T.unpack xs

defaultChatCompletionRequest :: ModelId -> [ChatMessage] -> ChatCompletionRequest
defaultChatCompletionRequest model messages =
  ChatCompletionRequest
    { chcrModel = model,
      chcrMessages = messages,
      chcrTools = Nothing,
      chcrToolChoice = Nothing,
      chcrTemperature = Nothing,
      chcrTopP = Nothing,
      chcrN = Nothing,
      chcrPrediction = Nothing,
      chcrSeed = Nothing,
      chcrStream = Nothing,
      chcrStop = Nothing,
      chcrMaxCompletionTokens = Nothing,
      chcrPresencePenalty = Nothing,
      chcrResponseFormat = Nothing,
      chcrFrequencyPenalty = Nothing,
      chcrLogitBias = Nothing,
      chcrUser = Nothing
    }

data ChatFinishReason
  = CFR_stop
  | CFR_length
  | CFR_contentFilter
  | CFR_toolCalls
  deriving (Show, Eq)

data ChatChoice = ChatChoice
  { chchIndex :: Int,
    chchMessage :: ChatMessage,
    chchFinishReason :: Maybe ChatFinishReason
  }
  deriving (Show, Eq)

data ChatResponse = ChatResponse
  { chrId :: Maybe T.Text,
    chrObject :: T.Text,
    chrCreated :: Int,
    chrModel :: T.Text,
    chrSystemFingerprint :: Maybe T.Text,
    chrChoices :: [ChatChoice],
    chrUsage :: Usage
  }

$(deriveJSON (jsonOpts 3) ''ChatFunction)
$(deriveJSON (jsonOpts 3) ''ChatTool)
$(deriveJSON (jsonOpts' 4) ''ChatPredictionType)
$(deriveJSON (jsonOpts 3) ''ChatPrediction)
$(deriveJSON (jsonOpts 4) ''ChatCompletionRequest)
$(deriveJSON (jsonOpts' 4) ''ChatFinishReason)
$(deriveJSON (jsonOpts 4) ''ChatChoice)
$(deriveJSON (jsonOpts 3) ''ChatResponse)

------------------------
------ Images API
------------------------

data ImageResponseData
  = IRD_url T.Text
  | IRD_b64_json T.Text
  deriving (Show, Eq)

instance ToJSON ImageResponseData where
  toJSON = \case
    IRD_url d -> A.object ["url" A..= A.String d]
    IRD_b64_json d -> A.object ["b64_json" A..= A.String d]

instance FromJSON ImageResponseData where
  parseJSON = A.withObject "ImageResponseData" $ \o ->
    (IRD_url <$> (o A..: "url"))
      <|> (IRD_b64_json <$> (o A..: "b64_json"))
      <|> fail "ImageResponseData unexpected data"

data ImageResponse = ImageResponse
  { irCreated :: TimeStamp,
    irData :: [ImageResponseData]
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''ImageResponse)

data ImageResponseFormat
  = IRF_url
  | IRF_b64_json
  deriving (Show, Eq)

instance ToJSON ImageResponseFormat where
  toJSON = \case
    IRF_url -> A.String "url"
    IRF_b64_json -> A.String "b64_json"

instance FromJSON ImageResponseFormat where
  parseJSON = A.withText "ImageResponseFormat" $ \t -> do
    case t of
      "url" ->
        pure IRF_url
      "b64_json" ->
        pure IRF_b64_json
      xs ->
        fail $ "ImageResponseFormat unexpected type: " <> T.unpack xs

-- | Image create API
data ImageCreate = ImageCreate
  { icPrompt :: T.Text,
    icModel :: Maybe ModelId,
    icN :: Maybe Int,
    icSize :: Maybe T.Text,
    icResponseFormat :: Maybe ImageResponseFormat,
    icUser :: Maybe T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''ImageCreate)

-- | Image edit API
data ImageEditRequest = ImageEditRequest
  { ierImage :: T.Text,
    ierMask :: Maybe T.Text,
    ierPrompt :: T.Text,
    ierN :: Maybe Int,
    ierSize :: Maybe T.Text,
    ierResponseFormat :: Maybe T.Text,
    ierUser :: Maybe T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''ImageEditRequest)

-- | Image variation API
data ImageVariationRequest = ImageVariationRequest
  { ivrImage :: T.Text,
    ivrN :: Maybe Int,
    ivrSize :: Maybe T.Text,
    ivrResponseFormat :: Maybe T.Text,
    ivrUser :: Maybe T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''ImageVariationRequest)

------------------------
------ Embeddings API
------------------------

data EmbeddingCreate = EmbeddingCreate
  { embcModel :: ModelId,
    embcInput :: T.Text, -- TODO (2023.02.23): Extend to allow taking in array of strings or token arrays
    embcUser :: Maybe T.Text
  }
  deriving (Show, Eq)

data EmbeddingResponseData = EmbeddingResponseData
  { embdObject :: T.Text,
    embdEmbedding :: V.Vector Double,
    embdIndex :: Int
  }
  deriving (Show, Eq)

data EmbeddingUsage = EmbeddingUsage
  { embuPromptTokens :: Int,
    embuTotalTokens :: Int
  }
  deriving (Show, Eq)

data EmbeddingResponse = EmbeddingResponse
  { embrObject :: T.Text,
    embrData :: [EmbeddingResponseData],
    embrModel :: ModelId,
    embrUsage :: EmbeddingUsage
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 4) ''EmbeddingCreate)
$(deriveJSON (jsonOpts 4) ''EmbeddingResponseData)
$(deriveJSON (jsonOpts 4) ''EmbeddingUsage)
$(deriveJSON (jsonOpts 4) ''EmbeddingResponse)

------------------------
------ Audio API
------------------------

data AudioResponseData = AudioResponseData
  { audrdText :: T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 5) ''AudioResponseData)

-- | Audio create API
data AudioTranscriptionRequest = AudioTranscriptionRequest
  { audtsrFile :: FilePath,
    audtsrModel :: ModelId,
    audtsrPrompt :: Maybe T.Text,
    audtsrResponseFormat :: Maybe T.Text,
    audtsrTemperature :: Maybe Double,
    audtsrLanguage :: Maybe T.Text
  }
  deriving (Show, Eq)

instance ToMultipart Tmp AudioTranscriptionRequest where
  toMultipart atr =
    MultipartData
      ( catMaybes
          [ Input "model" . unModelId <$> Just (audtsrModel atr),
            Input "prompt" <$> audtsrPrompt atr,
            Input "response_format" <$> audtsrResponseFormat atr,
            Input "temperature" . T.pack . show <$> audtsrTemperature atr,
            Input "language" <$> audtsrLanguage atr
          ]
      )
      [ FileData "file" (T.pack . audtsrFile $ atr) (T.decodeUtf8 . defaultMimeLookup . T.pack $ audtsrFile atr) (audtsrFile atr)
      ]

$(deriveJSON (jsonOpts 6) ''AudioTranscriptionRequest)

-- | Audio translation API
data AudioTranslationRequest = AudioTranslationRequest
  { audtlrFile :: FilePath,
    audtlrModel :: ModelId,
    audtlrPrompt :: Maybe T.Text,
    audtlrResponseFormat :: Maybe T.Text,
    audtlrTemperature :: Maybe Double
  }
  deriving (Show, Eq)

instance ToMultipart Tmp AudioTranslationRequest where
  toMultipart atr =
    MultipartData
      ( catMaybes
          [ Input "model" . unModelId <$> Just (audtlrModel atr),
            Input "prompt" <$> audtlrPrompt atr,
            Input "response_format" <$> audtlrResponseFormat atr,
            Input "temperature" . T.pack . show <$> audtlrTemperature atr
          ]
      )
      [ FileData "file" (T.pack . audtlrFile $ atr) (T.decodeUtf8 . defaultMimeLookup . T.pack $ audtlrFile atr) (audtlrFile atr)
      ]

$(deriveJSON (jsonOpts 6) ''AudioTranslationRequest)

------------------------
------ Files API
------------------------

data FineTuneHunk = FineTuneHunk
  { fthPrompt :: T.Text,
    fthCompletion :: T.Text
  }
  deriving (Show, Eq)

data FileHunk
  = FhFineTune FineTuneHunk
  deriving stock (Show, Eq)

$(deriveJSON (jsonOpts 3) ''FineTuneHunk)

newtype FileId = FileId {unFileId :: T.Text}
  deriving stock (Eq)
  deriving newtype (IsString, ToJSON, FromJSON, ToHttpApiData, Show)

data File = File
  { fId :: FileId,
    fObject :: T.Text,
    fBytes :: Int,
    fCreatedAt :: TimeStamp,
    fFilename :: T.Text,
    fPurpose :: T.Text
  }
  deriving stock (Show, Eq)

$(deriveJSON (jsonOpts 1) ''File)

-- | File upload API
data FileCreate = FileCreate
  { fcPurpose :: T.Text,
    fcDocuments :: [FileHunk]
  }
  deriving stock (Show, Eq)

packDocuments :: [FileHunk] -> BSL.ByteString
packDocuments docs =
  BSL.intercalate "\n" $
    map
      ( \t -> A.encode $
          case t of
            FhFineTune x -> A.toJSON x
      )
      docs

instance ToMultipart Mem FileCreate where
  toMultipart rfc =
    MultipartData
      [ Input "purpose" (fcPurpose rfc)
      ]
      [ FileData "file" "data.jsonl" "application/json" (packDocuments $ fcDocuments rfc)
      ]

-- | File delete API
newtype FileDeleteConfirmation = FileDeleteConfirmation
  { fdcId :: FileId
  }
  deriving stock (Eq)
  deriving newtype (IsString, Show)

$(deriveJSON (jsonOpts 3) ''FileDeleteConfirmation)

-- | File retrieve API
-- TODO

-- | File retrieve content API
-- TODO

------------------------
------ Engine API (deprecated)
------------------------

newtype EngineId = EngineId {unEngineId :: T.Text}
  deriving stock (Eq)
  deriving newtype (IsString, ToJSON, FromJSON, ToHttpApiData, Show)

data Engine = Engine
  { eId :: EngineId,
    eOwner :: T.Text,
    eReady :: Bool
  }
  deriving stock (Show, Eq)

$(deriveJSON (jsonOpts 1) ''Engine)

------------------------
------ Engine completions API (deprecated)
------------------------

newtype TextCompletionId = TextCompletionId {unTextCompletionId :: T.Text}
  deriving stock (Eq)
  deriving newtype (IsString, ToJSON, FromJSON, ToHttpApiData, Show)

data TextCompletionChoice = TextCompletionChoice
  { tccText :: T.Text,
    tccIndex :: Int,
    tccLogProps :: Maybe Int,
    tccFinishReason :: T.Text
  }
  deriving stock (Show, Eq)

data TextCompletion = TextCompletion
  { tcId :: TextCompletionId,
    tcCreated :: TimeStamp,
    tcModel :: T.Text,
    tcChoices :: V.Vector TextCompletionChoice
  }
  deriving stock (Show, Eq)

data TextCompletionCreate = TextCompletionCreate
  { tccrPrompt :: T.Text, -- TODO: support lists of strings
    tccrMaxTokens :: Maybe Int,
    tccrTemperature :: Maybe Double,
    tccrTopP :: Maybe Double,
    tccrN :: Maybe Int,
    tccrLogprobs :: Maybe Int,
    tccrEcho :: Maybe Bool,
    tccrStop :: Maybe (V.Vector T.Text),
    tccrPresencePenalty :: Maybe Double,
    tccrFrequencyPenalty :: Maybe Double,
    tccrBestOf :: Maybe Int
  }
  deriving stock (Show, Eq)

-- | Applies API defaults, only passing a prompt.
defaultEngineTextCompletionCreate :: T.Text -> TextCompletionCreate
defaultEngineTextCompletionCreate prompt =
  TextCompletionCreate
    { tccrPrompt = prompt,
      tccrMaxTokens = Nothing,
      tccrTemperature = Nothing,
      tccrTopP = Nothing,
      tccrN = Nothing,
      tccrLogprobs = Nothing,
      tccrEcho = Nothing,
      tccrStop = Nothing,
      tccrPresencePenalty = Nothing,
      tccrFrequencyPenalty = Nothing,
      tccrBestOf = Nothing
    }

$(deriveJSON (jsonOpts 3) ''TextCompletionChoice)
$(deriveJSON (jsonOpts 2) ''TextCompletion)
$(deriveJSON (jsonOpts 4) ''TextCompletionCreate)

------------------------
------ EngineEmbeddings API (deprecated)
------------------------

newtype EngineEmbeddingCreate = EngineEmbeddingCreate
  {enecInput :: T.Text}
  deriving stock (Eq)
  deriving newtype (IsString, Show)

data EngineEmbedding = EngineEmbedding
  {eneEmbedding :: V.Vector Double, eneIndex :: Int}
  deriving stock (Show, Eq)

$(deriveJSON (jsonOpts 4) ''EngineEmbeddingCreate)
$(deriveJSON (jsonOpts 3) ''EngineEmbedding)

------------------------
------ Old stuff; not touching
------ TODO 2023.03.22: Not touching this; unchanged since last year
------------------------

newtype FineTuneId = FineTuneId {unFineTuneId :: T.Text}
  deriving stock (Eq)
  deriving newtype (IsString, ToJSON, FromJSON, ToHttpApiData, Show)

data FineTuneCreate = FineTuneCreate
  { ftcTrainingFile :: FileId,
    ftcValidationFile :: Maybe FileId,
    ftcModel :: Maybe T.Text,
    ftcBatchSize :: Maybe Int,
    ftcNEpochs :: Maybe T.Text,
    ftcLearningRateMultiplier :: Maybe Double,
    ftcPromptLossWeight :: Maybe Double,
    ftcComputeClassificationMetrics :: Maybe Bool,
    ftcClassificationNClasses :: Maybe Int,
    ftcClassificationPositiveClass :: Maybe T.Text
  }
  deriving stock (Show, Eq)

defaultFineTuneCreate :: FileId -> FineTuneCreate
defaultFineTuneCreate file =
  FineTuneCreate
    { ftcTrainingFile = file,
      ftcValidationFile = Nothing,
      ftcModel = Nothing,
      ftcBatchSize = Nothing,
      ftcNEpochs = Nothing,
      ftcLearningRateMultiplier = Nothing,
      ftcPromptLossWeight = Nothing,
      ftcComputeClassificationMetrics = Nothing,
      ftcClassificationNClasses = Nothing,
      ftcClassificationPositiveClass = Nothing
    }

data FineTuneEvent = FineTuneEvent
  { fteCreatedAt :: Int,
    fteLevel :: T.Text,
    fteMessage :: T.Text
  }
  deriving stock (Show, Eq)

data FineTune = FineTune
  { ftId :: FineTuneId,
    ftModel :: T.Text,
    ftCreatedAt :: Int,
    ftEvents :: V.Vector FineTuneEvent,
    ftTunedModel :: Maybe T.Text,
    ftStatus :: T.Text
  }
  deriving stock (Show, Eq)

$(deriveJSON (jsonOpts 3) ''FineTuneCreate)
$(deriveJSON (jsonOpts 3) ''FineTuneEvent)
$(deriveJSON (jsonOpts 2) ''FineTune)

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Google.Resources where

import Common.Internal.Aeson
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V

data ChatRole
  = CR_user
  | CR_model
  deriving (Show, Eq)

$(deriveJSON (jsonOpts' 3) ''ChatRole)

data GeneratePartFuncCall = GeneratePartFuncCall
  { gpfcName :: T.Text,
    gpfcArgs :: A.Object
  }
  deriving (Show, Eq)

data GeneratePartFuncResponse = GeneratePartFuncResponse
  { gpfrName :: T.Text,
    gpfrResponse :: A.Value
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 4) ''GeneratePartFuncCall)
$(deriveJSON (jsonOpts 4) ''GeneratePartFuncResponse)

data GenerateInlineData = GenerateInlineData
  { gid_data :: T.Text,
    gid_mimeType :: T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts'' 4) ''GenerateInlineData)

data GeneratePart = GeneratePart
  { gp_text :: Maybe T.Text,
    gp_functionCall :: Maybe GeneratePartFuncCall,
    gp_functionResponse :: Maybe GeneratePartFuncResponse,
    gp_thought :: Maybe Bool,
    gp_inlineData :: Maybe GenerateInlineData
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts'' 3) ''GeneratePart)

data GenerateContent = GenerateContent
  { gcRole :: Maybe ChatRole,
    gcParts :: [GeneratePart]
  }
  deriving (Show, Eq)

data HarmCategory
  = HC_HARM_CATEGORY_HARASSMENT
  | HC_HARM_CATEGORY_HATE_SPEECH
  | HC_HARM_CATEGORY_SEXUALLY_EXPLICIT
  | HC_HARM_CATEGORY_DANGEROUS_CONTENT
  | HC_HARM_CATEGORY_CIVIC_INTEGRITY
  deriving (Show, Eq, Enum, Bounded)

$(deriveJSON (jsonOpts'' 3) ''HarmCategory)

data HarmBlockThreahold
  = HBT_HARM_BLOCK_THRESHOLD_UNSPECIFIED
  | HBT_BLOCK_LOW_AND_ABOVE
  | HBT_BLOCK_MEDIUM_AND_ABOVE
  | HBT_BLOCK_ONLY_HIGH
  | HBT_BLOCK_NONE
  | HBT_OFF
  deriving (Show, Eq)

$(deriveJSON (jsonOpts'' 4) ''HarmBlockThreahold)

data SafetySetting = SafetySetting
  { ssThreshold :: HarmBlockThreahold,
    ssCategory :: HarmCategory
  }
  deriving (Show, Eq)

data ChatTool = ChatTool
  { chtName :: T.Text,
    chtDescription :: T.Text,
    chtParameters :: A.Value,
    chtRequired :: Maybe [T.Text]
  }
  deriving (Show, Eq)

data FunctionDeclarations = FunctionDeclarations
  { fd_function_declarations :: [ChatTool]
  }
  deriving (Show, Eq)

data ThinkingConfig = ThinkingConfig
  { tc_includeThoughts :: Maybe Bool,
    tc_thinkingBudget :: Maybe Int
  }
  deriving (Show, Eq)

data ResponseModality = RM_Text | RM_Image
  deriving (Show, Eq)

$(deriveJSON (jsonOpts'' 3) ''ResponseModality)

data GenerateContentConfig = GenerateContentConfig
  { gcc_thinkingConfig :: Maybe ThinkingConfig,
    gcc_responseModalities :: Maybe [ResponseModality],
    gcc_responseMimeType :: T.Text
  }
  deriving (Show, Eq)

data ImagenInstance = ImagenInstance
  { ii_prompt :: T.Text
  }
  deriving (Show, Eq)

data ImagenParameters = ImagenParameters
  { ip_sampleCount :: Int
  }
  deriving (Show, Eq)

data GenerateContentRequest = GenerateContentRequest
  { gcr_contents :: Maybe [GenerateContent],
    gcr_system_instruction :: Maybe GenerateContent,
    gcr_safetySettings :: Maybe [SafetySetting],
    gcr_model :: T.Text,
    gcr_tools :: Maybe [FunctionDeclarations],
    gcr_generationConfig :: Maybe GenerateContentConfig,
    -- Imagen
    gcr_instances :: Maybe [ImagenInstance],
    gcr_parameters :: Maybe ImagenParameters
  }
  deriving (Show, Eq)

data GenerateCandidate = GenerateCandidate
  { gnContent :: GenerateContent
  }
  deriving (Show, Eq)

data GeneratePrediction = GeneratePrediction
  { gp_bytesBase64Encoded :: T.Text,
    gp_mimeType :: T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts'' 3) ''GeneratePrediction)

data GenerateResponse = GenerateResponse
  { gr_candidates :: Maybe (V.Vector GenerateCandidate),
    gr_predictions :: Maybe (V.Vector GeneratePrediction),
    gr_modelVersion :: Maybe T.Text,
    gr_usageMetadata :: Maybe A.Value,
    gr_responseId :: Maybe T.Text,
    gr_promptFeedback :: Maybe A.Object
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''GenerateContent)
$(deriveJSON (jsonOpts 2) ''SafetySetting)
$(deriveJSON (jsonOpts 3) ''ChatTool)
$(deriveJSON (jsonOpts 3) ''FunctionDeclarations)
$(deriveJSON (jsonOpts'' 3) ''ThinkingConfig)
$(deriveJSON (jsonOpts'' 4) ''GenerateContentConfig)
$(deriveJSON (jsonOpts'' 3) ''ImagenInstance)
$(deriveJSON (jsonOpts'' 3) ''ImagenParameters)
$(deriveJSON (jsonOpts'' 4) ''GenerateContentRequest)
$(deriveJSON (jsonOpts 2) ''GenerateCandidate)
$(deriveJSON (jsonOpts'' 3) ''GenerateResponse)

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

data GeneratePart = GeneratePart
  { gp_text :: Maybe T.Text,
    gp_functionCall :: Maybe GeneratePartFuncCall,
    gp_functionResponse :: Maybe GeneratePartFuncResponse
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

data GenerateContentRequest = GenerateContentRequest
  { gcr_contents :: [GenerateContent],
    gcr_system_instruction :: Maybe GenerateContent,
    gcr_safetySettings :: Maybe [SafetySetting],
    gcr_model :: T.Text,
    gcr_tools :: Maybe [FunctionDeclarations]
  }
  deriving (Show, Eq)

data GenerateCandidate = GenerateCandidate
  { gnContent :: GenerateContent
  }
  deriving (Show, Eq)

data GenerateResponse = GenerateResponseb
  { grCandidates :: V.Vector GenerateCandidate
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''GenerateContent)
$(deriveJSON (jsonOpts 2) ''SafetySetting)
$(deriveJSON (jsonOpts 3) ''ChatTool)
$(deriveJSON (jsonOpts 3) ''FunctionDeclarations)
$(deriveJSON (jsonOpts'' 4) ''GenerateContentRequest)
$(deriveJSON (jsonOpts 2) ''GenerateCandidate)
$(deriveJSON (jsonOpts 2) ''GenerateResponse)

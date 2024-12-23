{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Google.Resources where

import Common.Internal.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V

data ChatRole
  = CR_user
  | CR_model
  deriving (Show, Eq)

$(deriveJSON (jsonOpts' 3) ''ChatRole)

data GeneratePart = GeneratePart
  { gpText :: T.Text
  }
  deriving (Show, Eq)

data GenerateContent = GenerateContent
  { gcRole :: Maybe ChatRole,
    gcParts :: V.Vector GeneratePart
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

data GenerateContentRequest = GenerateContentRequest
  { gcr_contents :: V.Vector GenerateContent,
    gcr_system_instruction :: Maybe GenerateContent,
    gcr_safetySettings :: Maybe [SafetySetting],
    gcr_model :: T.Text
  }
  deriving (Show, Eq)

-- data GenerateContent = GenerateContent
--   { gcParts :: V.Vector Part
--   }
--   deriving (Show, Eq)

data GenerateCandidate = GenerateCandidate
  { gnContent :: GenerateContent
  }
  deriving (Show, Eq)

data GenerateResponse = GenerateResponse
  { grCandidates :: V.Vector GenerateCandidate
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''GeneratePart)
$(deriveJSON (jsonOpts 2) ''GenerateContent)
$(deriveJSON (jsonOpts 2) ''SafetySetting)
$(deriveJSON (jsonOpts'' 4) ''GenerateContentRequest)

-- $(deriveJSON (jsonOpts 2) ''GenerateContent)

$(deriveJSON (jsonOpts 2) ''GenerateCandidate)
$(deriveJSON (jsonOpts 2) ''GenerateResponse)

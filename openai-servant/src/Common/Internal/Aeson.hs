module Common.Internal.Aeson (jsonOpts, jsonOpts', deriveJSON, ToJSON, FromJSON) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Text.Casing (quietSnake)

jsonOpts :: Int -> Options
jsonOpts x =
  defaultOptions
    { fieldLabelModifier = quietSnake . drop x,
      constructorTagModifier = quietSnake,
      omitNothingFields = True
    }

jsonOpts' :: Int -> Options
jsonOpts' x =
  defaultOptions
    { fieldLabelModifier = quietSnake . drop x,
      constructorTagModifier = quietSnake . drop x,
      omitNothingFields = True,
      tagSingleConstructors = True
    }

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module PlutusTx.Blueprint.Argument where

import Prelude

import Data.Aeson (KeyValue ((.=)), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Extra
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Function ((&))
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import PlutusTx.Blueprint.Purpose (Purpose)
import PlutusTx.Blueprint.Schema (Schema)

-- | Blueprint that defines a validator's runtime argument: datum or redeemer.
data ArgumentBlueprint (referencedTypes :: [Type]) = MkArgumentBlueprint
  { argumentTitle       :: Maybe Text
  -- ^ A short and descriptive name for the redeemer or datum.
  , argumentDescription :: Maybe Text
  -- ^ An informative description of the redeemer or datum.
  , argumentPurpose     :: Set Purpose
  -- ^ A possibly empty set of purposes for the redeemer or datum.
  , argumentSchema      :: Schema referencedTypes
  -- ^ A Plutus Data Schema.
  }
  deriving stock (Show, Eq)

instance ToJSON (ArgumentBlueprint referencedTypes) where
  toJSON MkArgumentBlueprint{..} =
    KeyMap.empty
      & optionalField "title" argumentTitle
      & optionalField "description" argumentDescription
      & optionalField "purpose" purpose
      & requiredField "schema" argumentSchema
      & Aeson.Object
   where
    purpose :: Maybe Aeson.Value =
      case Set.toList argumentPurpose of
        []  -> Nothing
        [x] -> Just $ toJSON x
        xs  -> Just $ Aeson.object ["oneOf" .= xs]
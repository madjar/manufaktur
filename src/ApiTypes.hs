{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ApiTypes where

import Types

import RIO
import qualified RIO.Text as Text

import qualified Text.Regex.Applicative.Text as Regex
import Lens.Micro.TH
import Data.Aeson.TH
import Data.Aeson
import Data.Aeson.Yak

data Dependency = Dependency
  { dependencyOption :: Bool
  , dependencyName :: Text
  , dependencyConstraint :: Maybe Text
  } deriving (Show)
makeFields ''Dependency

instance FromJSON Dependency where
  parseJSON = withText "dependency" $ \s -> maybe (fail ("Could not parse " <> show s)) pure (Regex.match regex s)
      where regex = Dependency <$> optionR <*> nameR <*> constraintR
            optionR = isJust <$> optional (Regex.string "? ")
            nameR = Text.pack <$> some (Regex.psym (/= ' '))
            constraintR = optional $ Text.pack <$ Regex.string " >= " <*> some Regex.anySym


data ModInfo = ModInfo
  { modInfoName :: Text
  , modInfoVersion :: Text
  , modInfoFactorioVersion :: Text
  , modInfoDependencies :: Yak Dependency
  } deriving (Show)
makeFields ''ModInfo
deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop (Text.length "modInfo")} '' ModInfo
